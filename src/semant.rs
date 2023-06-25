// TODO remove this setting to make compiler STFU
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use cfgrammar::Span;
use std::collections::HashMap;
use std::{cell::RefCell, num::NonZeroUsize, rc::Rc};

use crate::{
    absyn::{Dec, Exp, Field, Fundec, Oper, Ty, TyDec, Var},
    frame,
    frame::Frame,
    symbol::{Interner, Symbol},
    symtab::SymbolTable,
    temp::{GenTemporary, Label, Temp},
    translate,
    translate::{Level, TrExp, ERROR_TR_EXP},
};
use strum_macros::Display;

pub struct TypeCheckingContext<'a> {
    next_array_record_ord: NonZeroUsize,
    type_env: SymbolTable<Type>,
    varfun_env: SymbolTable<EnvEntry>,
    has_err: bool,
    symbols: Interner,
    input: &'a str,
    gen_temp_label: GenTemporary,
}

impl<'a> TypeCheckingContext<'a> {
    fn new(input: &'a str) -> Self {
        let mut gen_temp_label = GenTemporary::new();
        let mut symbols = Interner::new();
        let type_env = Self::base_env_type_env(&mut symbols);
        let varfun_env = Self::base_varfun_env(&mut symbols, &mut gen_temp_label);
        Self {
            next_array_record_ord: NonZeroUsize::MIN,
            type_env: type_env,
            varfun_env: varfun_env,
            has_err: false,
            symbols: symbols,
            input: input,
            gen_temp_label: gen_temp_label,
        }
    }

    fn get_next_array_record_ord(&mut self) -> NonZeroUsize {
        // This will panic if we wrap around. It is unlikely as we would have to exhaust
        // the size of a usize first. Practically impossible on 64bit ints.
        let ret = self.next_array_record_ord;
        self.next_array_record_ord =
            NonZeroUsize::new(self.next_array_record_ord.get().wrapping_add(1)).unwrap();
        ret
    }

    fn base_env_type_env(pool: &mut Interner) -> SymbolTable<Type> {
        let mut res = SymbolTable::empty();
        res.begin_scope();
        res.enter(pool.intern("int"), Type::Int);
        res.enter(pool.intern("string"), Type::String);
        res
    }

    fn base_varfun_env(pool: &mut Interner, la: &mut GenTemporary) -> SymbolTable<EnvEntry> {
        let mut res = SymbolTable::empty();
        res.begin_scope();
        res.enter(
            pool.intern("print"),
            EnvEntry::FunEntry {
                level: Level::outermost(),
                formals: Rc::new(vec![Type::String]),
                result: Type::String,
                label: la.new_label(pool),
            },
        );
        res.enter(
            pool.intern("flush"),
            EnvEntry::FunEntry {
                level: Level::outermost(),
                formals: Rc::new(vec![]),
                result: Type::Unit,
                label: la.new_label(pool),
            },
        );
        res.enter(
            pool.intern("getchar"),
            EnvEntry::FunEntry {
                level: Level::outermost(),
                formals: Rc::new(vec![]),
                result: Type::String,
                label: la.new_label(pool),
            },
        );
        res.enter(
            pool.intern("ord"),
            EnvEntry::FunEntry {
                level: Level::outermost(),
                formals: Rc::new(vec![Type::String]),
                result: Type::Int,
                label: la.new_label(pool),
            },
        );
        res.enter(
            pool.intern("chr"),
            EnvEntry::FunEntry {
                level: Level::outermost(),
                formals: Rc::new(vec![Type::Int]),
                result: Type::String,
                label: la.new_label(pool),
            },
        );
        res.enter(
            pool.intern("size"),
            EnvEntry::FunEntry {
                level: Level::outermost(),
                formals: Rc::new(vec![Type::String]),
                result: Type::Int,
                label: la.new_label(pool),
            },
        );
        res.enter(
            pool.intern("substring"),
            EnvEntry::FunEntry {
                level: Level::outermost(),
                formals: Rc::new(vec![Type::String, Type::Int, Type::Int]),
                result: Type::String,
                label: la.new_label(pool),
            },
        );
        res.enter(
            pool.intern("concat"),
            EnvEntry::FunEntry {
                level: Level::outermost(),
                formals: Rc::new(vec![Type::String, Type::String]),
                result: Type::String,
                label: la.new_label(pool),
            },
        );
        res.enter(
            pool.intern("not"),
            EnvEntry::FunEntry {
                level: Level::outermost(),
                formals: Rc::new(vec![Type::Int]),
                result: Type::Int,
                label: la.new_label(pool),
            },
        );
        res.enter(
            pool.intern("exit"),
            EnvEntry::FunEntry {
                level: Level::outermost(),
                formals: Rc::new(vec![Type::Int]),
                result: Type::Unit,
                label: la.new_label(pool),
            },
        );
        res
    }

    fn flag_error_with_msg(&mut self, msg: &str) {
        self.has_err = true;
        println!("{}", msg);
    }

    fn flag_error(&mut self) {
        self.has_err = true;
    }

    fn get_span(&self, s: &Span) -> &str {
        &self.input[s.start()..s.end()]
    }

    fn intern(&mut self, s: &Span) -> Symbol {
        // cannot call get_span here because it will borrow the self parameter as a immutable ref.
        // otoh, we can borrow the self.input here since it's a "separate" chunk of memory.
        // fucking pita.
        let x = &self.input[s.start()..s.end()];
        self.symbols.intern(x)
    }

    fn has_error(&self) -> bool {
        self.has_err
    }

    fn resolve_unchecked(&self, s: &Symbol) -> &str {
        self.symbols.resolve(s).unwrap()
    }
}

#[derive(Eq, PartialEq, Display, Clone)]
pub enum Type {
    Record(Rc<Vec<(Symbol, Type)>>, NonZeroUsize),
    Nil,
    Int,
    String,
    Array(Rc<Box<Type>>, NonZeroUsize),
    Unit,
    Name(Symbol),
    Error,
}

impl Type {
    fn compatible_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Nil, Type::Record(_, _)) | (Type::Record(_, _), Type::Nil) => true,
            (a, b) => a == b,
        }
    }
}

// The translated IR and the result of the type check.
type ExpTy = (TrExp, Type);

#[derive(Clone, Display)]
enum EnvEntry {
    VarEntry {
        access: translate::Access,
        ty: Type,
        readonly: bool,
    },
    FunEntry {
        level: Rc<RefCell<Level>>,
        label: Label,
        formals: Rc<Vec<Type>>,
        result: Type,
    },
}

fn trans_exp<T: Frame + 'static>(
    ctx: &mut TypeCheckingContext,
    level: Rc<RefCell<Level>>,
    n: &Exp,
    break_label: Option<Label>,
) -> ExpTy {
    match n {
        Exp::OpExp {
            left,
            right,
            oper,
            pos,
        } => {
            let (lhs_ir, lhs_ty) = trans_exp::<T>(ctx, level.clone(), left, break_label);
            let (rhs_ir, rhs_ty) = trans_exp::<T>(ctx, level.clone(), right, break_label);

            match oper {
                Oper::PlusOp | Oper::MinusOp | Oper::TimesOp | Oper::DivideOp => {
                    match (lhs_ty, rhs_ty) {
                        (Type::Int, Type::Int) => {
                            (translate::binop(oper, lhs_ir, rhs_ir), Type::Int)
                        }
                        (Type::Error, _) | (_, Type::Error) => (ERROR_TR_EXP, Type::Error),
                        (Type::Int, _) => {
                            ctx.flag_error_with_msg(&format!(
                                "Expected integer on rhs but got {}",
                                right
                            ));
                            (ERROR_TR_EXP, Type::Error)
                        }
                        (_, Type::Int) => {
                            ctx.flag_error_with_msg(&format!(
                                "Expected integer on lhs but got {}",
                                left
                            ));
                            (ERROR_TR_EXP, Type::Error)
                        }
                        (_, _) => {
                            ctx.flag_error_with_msg(&format!(
                                "Expected integer operands but got lhs={}, rhs={}",
                                left, right
                            ));
                            (ERROR_TR_EXP, Type::Error)
                        }
                    }
                }
                Oper::LtOp | Oper::LeOp | Oper::GtOp | Oper::GeOp => match (lhs_ty, rhs_ty) {
                    (Type::Int, Type::Int) => (translate::binop(oper, lhs_ir, rhs_ir), Type::Int),
                    (Type::Error, _) | (_, Type::Error) => (ERROR_TR_EXP, Type::Error),
                    (Type::Int, _) => {
                        ctx.flag_error_with_msg(&format!(
                            "Expected integer on rhs but got {}",
                            right
                        ));
                        (ERROR_TR_EXP, Type::Error)
                    }
                    (_, Type::Int) => {
                        ctx.flag_error_with_msg(&format!(
                            "Expected integer on lhs but got {}",
                            right
                        ));
                        (ERROR_TR_EXP, Type::Error)
                    }
                    (_, _) => (ERROR_TR_EXP, Type::Error),
                },
                Oper::EqOp | Oper::NeqOp => {
                    if Type::Error == lhs_ty || Type::Error == rhs_ty {
                        (ERROR_TR_EXP, Type::Error)
                    } else if !lhs_ty.compatible_with(&rhs_ty) {
                        ctx.flag_error_with_msg(&format!(
                            "Types not compatible for comparison, lhs={}, rhs={}",
                            left, right
                        ));
                        (ERROR_TR_EXP, Type::Error)
                    } else if !matches!(
                        lhs_ty,
                        Type::Int | Type::Record(..) | Type::Array(..) | Type::String
                    ) || !matches!(
                        rhs_ty,
                        Type::Int | Type::Record(..) | Type::Array(..) | Type::String
                    ) {
                        ctx.flag_error_with_msg(&format!("{} only valid on Int, Record, Array or String types, but is used for {} and {}", oper, left, right));
                        (ERROR_TR_EXP, Type::Error)
                    } else {
                        if let (Type::String, Type::String) = (lhs_ty, rhs_ty) {
                            (
                                translate::string_cmp(matches!(oper, Oper::EqOp), lhs_ir, rhs_ir),
                                Type::Int,
                            )
                        } else {
                            (translate::binop(oper, lhs_ir, rhs_ir), Type::Int)
                        }
                    }
                }
            }
        }
        Exp::NilExp => (translate::nil_exp(), Type::Nil),
        Exp::IntExp(i) => (translate::int_exp(*i), Type::Int),
        Exp::VarExp(v) => trans_var::<T>(ctx, level.clone(), v, break_label),
        Exp::StringExp(s, pos) => (translate::string_exp(ctx.get_span(s)), Type::String),
        Exp::CallExp { func, args, pos } => {
            let sym = ctx.intern(func);
            let fentry_opt = ctx.varfun_env.look(sym).map(|e| e.clone());

            match fentry_opt {
                None => {
                    ctx.flag_error_with_msg(&format!(
                        "Trying to call undeclared function {}",
                        ctx.get_span(func)
                    ));
                    (ERROR_TR_EXP, Type::Error)
                }
                Some(EnvEntry::FunEntry {
                    formals, result, ..
                }) => {
                    if formals.len() != args.len() {
                        ctx.flag_error_with_msg(&format!(
                            "Expected {} args for function {} but got {}",
                            formals.len(),
                            ctx.get_span(func),
                            args.len()
                        ));
                        return (ERROR_TR_EXP, Type::Error);
                    } else {
                        let mut arg_irs = Vec::new();

                        for i in 0..args.len() {
                            let (arg_ir, arg_ty) =
                                trans_exp::<T>(ctx, level.clone(), args.get(i).unwrap(), None);
                            if matches!(arg_ty, Type::Error)
                                || matches!(formals.get(i).unwrap(), Type::Error)
                            {
                                ctx.flag_error();
                                break;
                            } else if !arg_ty.compatible_with(formals.get(i).unwrap()) {
                                ctx.flag_error_with_msg(&format!(
                                "Call to {} expects argument of type {} at position {} but got {}",
                                ctx.get_span(func),
                                formals.get(i).unwrap(),
                                i + 1,
                                arg_ty
                            ));
                            }
                            arg_irs.push(arg_ir);
                        }
                        if ctx.has_error() {
                            (ERROR_TR_EXP, Type::Error)
                        } else {
                            (translate::call_exp(), result.clone())
                        }
                    }
                }
                Some(_) => {
                    ctx.flag_error_with_msg(&format!("{} is not a function!", ctx.get_span(func)));
                    (ERROR_TR_EXP, Type::Error)
                }
            }
        }
        Exp::RecordExp { fields, typ, pos } => {
            let sym = ctx.intern(typ);
            let rec_entry_opt = ctx.type_env.look(sym).map(|e| e.clone());

            match rec_entry_opt {
                None => {
                    ctx.flag_error_with_msg(&format!(
                        "{} has not been declared.",
                        ctx.get_span(typ)
                    ));
                    (ERROR_TR_EXP, Type::Error)
                }
                Some(Type::Record(name_types, ord)) => {
                    if name_types.len() != fields.len() {
                        ctx.flag_error_with_msg(&format!(
                        "Record {} is declared with {} fields, but {} are given at instantiation site",
                        ctx.get_span(typ),
                        name_types.len(),
                        fields.len()
                    ));
                        (ERROR_TR_EXP, Type::Error)
                    } else {
                        let mut site_irs = Vec::new();

                        for (decl_sym, decl_typ) in name_types.as_ref() {
                            let mut found = false;
                            for (site_sym_span, site_typ_exp, _) in fields {
                                let site_sym = ctx.intern(site_sym_span);
                                if *decl_sym == site_sym {
                                    found = true;
                                    let (site_ir, site_ty) = trans_exp::<T>(
                                        ctx,
                                        level.clone(),
                                        site_typ_exp,
                                        break_label,
                                    );
                                    if !site_ty.compatible_with(decl_typ) {
                                        ctx.flag_error();
                                        if !matches!(Type::Error, site_ty) {
                                            ctx.flag_error_with_msg(&format!("field {} is declared with type {} but is used with type {}", ctx.get_span(site_sym_span), decl_typ, site_ty));
                                        } else {
                                            // only push when no type error occur.
                                            // this means we can check length vs the declared number of record fields
                                            // to see if type check error happened.
                                            site_irs.push(site_ir);
                                        }
                                        break;
                                    }
                                }
                                if !found {
                                    ctx.flag_error_with_msg(&format!(
                                        "record field {} is declared but not used",
                                        ctx.resolve_unchecked(decl_sym)
                                    ));
                                }
                            }
                        }

                        if site_irs.len() != fields.len() {
                            (ERROR_TR_EXP, Type::Error)
                        } else {
                            (
                                translate::record_exp(site_irs),
                                Type::Record(name_types.clone(), ord.clone()),
                            )
                        }
                    }
                }
                _ => {
                    ctx.flag_error_with_msg(&format!(
                        "{} is not a record type.",
                        ctx.get_span(typ)
                    ));
                    (ERROR_TR_EXP, Type::Error)
                }
            }
        }
        Exp::SeqExp(exps) => {
            let mut ret_val = Type::Unit;
            let mut exp_irs = Vec::new();
            for exp in exps {
                let (exp_ir, exp_ty) = trans_exp::<T>(ctx, level.clone(), exp, break_label);
                ret_val = exp_ty;
                exp_irs.push(exp_ir);
            }
            (
                translate::seq_exp(exp_irs, !matches!(ret_val, Type::Unit)),
                ret_val,
            )
        }
        Exp::AssignExp { var, exp, pos } => {
            // nil can be assigned to record types.
            let (dst_ir, dst_ty) = trans_var::<T>(ctx, level.clone(), var, break_label);
            let (src_ir, src_ty) = trans_exp::<T>(ctx, level.clone(), exp, break_label);

            if matches!(dst_ty, Type::Error) || matches!(src_ty, Type::Error) {
                (ERROR_TR_EXP, Type::Error)
            } else if matches!(src_ty, Type::Nil) && !matches!(dst_ty, Type::Record(..)) {
                ctx.flag_error_with_msg(&format!(
                    "Nil can only be assigned to record type, but is assigned to {} here.",
                    dst_ty
                ));
                (ERROR_TR_EXP, Type::Error)
            } else if !dst_ty.compatible_with(&src_ty) {
                ctx.flag_error_with_msg(&format!(
                    "Assigning incompatible types: dst={}, src={}",
                    dst_ty, src_ty
                ));
                (ERROR_TR_EXP, Type::Error)
            } else {
                match var.as_ref() {
                    Var::SimpleVar(span, pos) => {
                        let sym = ctx.intern(span);
                        let var_entry = ctx.varfun_env.look(sym).map(|e| e.clone());

                        match var_entry {
                            None => {
                                ctx.flag_error_with_msg(&format!(
                                    "the variable {} needs to be declared before being assigned to",
                                    ctx.get_span(span)
                                ));
                                (ERROR_TR_EXP, Type::Error)
                            }
                            Some(EnvEntry::FunEntry { .. }) => {
                                ctx.flag_error_with_msg("Cannot assign to a function");
                                (ERROR_TR_EXP, Type::Error)
                            }
                            Some(EnvEntry::VarEntry { ty, readonly, .. }) => {
                                if readonly {
                                    ctx.flag_error_with_msg("Cannot assign to readonly location");
                                    (ERROR_TR_EXP, Type::Error)
                                } else if matches!(ty, Type::Error) {
                                    (ERROR_TR_EXP, Type::Error)
                                } else {
                                    (translate::assignment(dst_ir, src_ir), Type::Unit)
                                }
                            }
                        }
                    }
                    _ => (translate::assignment(dst_ir, src_ir), Type::Unit),
                }
            }
        }
        Exp::IfExp {
            test,
            then,
            els,
            pos,
        } => {
            let (cond_ir, test_ty) = trans_exp::<T>(ctx, level.clone(), test.as_ref(), break_label);
            if !matches!(test_ty, Type::Int) {
                ctx.flag_error_with_msg(&format!(
                    "Conditionals must evaluate to INT but got {} here",
                    test_ty
                ));
                (ERROR_TR_EXP, Type::Error)
            } else {
                let (then_ir, then_ty) =
                    trans_exp::<T>(ctx, level.clone(), then.as_ref(), break_label);
                if els.is_none() {
                    if !matches!(then_ty, Type::Unit) {
                        ctx.flag_error_with_msg(&format!(
                            "if-then must evaluate to Unit, but has type {} here",
                            then_ty
                        ));
                        (ERROR_TR_EXP, Type::Error)
                    } else {
                        (translate::conditional(cond_ir, then_ir, None), then_ty)
                    }
                } else {
                    let (else_ir, else_ty) =
                        trans_exp::<T>(ctx, level.clone(), els.as_ref().unwrap(), break_label);
                    if !else_ty.compatible_with(&then_ty) {
                        ctx.flag_error_with_msg(&format!("if-then-else branches have incompatible types: then has type {}, else has type {}", then_ty, else_ty));
                        (ERROR_TR_EXP, Type::Error)
                    } else {
                        (
                            translate::conditional(cond_ir, then_ir, Some(else_ir)),
                            then_ty,
                        )
                    }
                }
            }
        }
        Exp::WhileExp { test, body, pos } => {
            let while_done_label = Some(ctx.gen_temp_label.new_label(&mut ctx.symbols));
            // break is legal in expressions. so if they try to break in the condition
            // of a while loop, it is interpreted as breaking to the end of this while loop,
            // since logically, the while condition is re-evaluated each iteration.
            let (cond_ir, cond_ty) = trans_exp::<T>(ctx, level.clone(), test, while_done_label);
            if !matches!(cond_ty, Type::Int) {
                ctx.flag_error_with_msg("while condition must be of Int");
                (ERROR_TR_EXP, Type::Error)
            } else {
                let (body_ir, body_ty) = trans_exp::<T>(ctx, level.clone(), body, while_done_label);
                match body_ty {
                    Type::Unit => (
                        translate::while_loop(cond_ir, body_ir, while_done_label.unwrap()),
                        Type::Unit,
                    ),
                    _ => {
                        ctx.flag_error_with_msg("while body must not produce a value");
                        (ERROR_TR_EXP, Type::Error)
                    }
                }
            }
        }
        Exp::ForExp {
            var,
            escape,
            lo,
            hi,
            body,
            pos,
        } => {
            let for_done_label = Some(ctx.gen_temp_label.new_label(&mut ctx.symbols));
            // if "break" happens in evaluating the for loop params, will just break the loop itself.
            let (lo_ir, lo_ty) = trans_exp::<T>(ctx, level.clone(), lo, for_done_label);
            let (hi_ir, hi_ty) = trans_exp::<T>(ctx, level.clone(), hi, for_done_label);
            let mut err = false;
            if !matches!(lo_ty, Type::Int) {
                err = true;
                ctx.flag_error_with_msg("lo in for loop range must be of integral type");
            }
            if !matches!(hi_ty, Type::Int) {
                err = true;
                ctx.flag_error_with_msg("lo in for loop range must be of integral type");
            }

            ctx.varfun_env.begin_scope();

            let sym = ctx.intern(var);
            let acc = level.as_ref().borrow_mut().alloc_local(*escape);

            ctx.varfun_env.enter(
                sym,
                EnvEntry::VarEntry {
                    access: acc,
                    ty: Type::Int,
                    readonly: true,
                },
            );
            let (body_ir, body_ty) = trans_exp::<T>(ctx, level.clone(), body, for_done_label);
            ctx.varfun_env.end_scope();

            match body_ty {
                Type::Unit => {
                    if err {
                        (ERROR_TR_EXP, Type::Error)
                    } else {
                        (
                            translate::for_loop(lo_ir, hi_ir, body_ir, for_done_label.unwrap()),
                            Type::Unit,
                        )
                    }
                }
                _ => {
                    ctx.flag_error_with_msg("for body must not produce a value");
                    (ERROR_TR_EXP, Type::Error)
                }
            }
        }
        Exp::BreakExp(pos) => {
            if break_label.is_none() {
                ctx.flag_error_with_msg("naked break statement");
                (ERROR_TR_EXP, Type::Error)
            } else {
                (translate::break_stmt(break_label.unwrap()), Type::Unit)
            }
        }
        Exp::LetExp { decs, body, pos } => {
            ctx.type_env.begin_scope();
            ctx.varfun_env.begin_scope();

            let mut var_init_irs = Vec::new();
            for dec in decs {
                if let Some(var_init_ir) = trans_dec::<T>(ctx, level.clone(), dec, break_label) {
                    var_init_irs.push(var_init_ir);
                }
            }
            let (let_body_ir, let_body_ty) = trans_exp::<T>(ctx, level.clone(), body, break_label);

            ctx.type_env.end_scope();
            ctx.varfun_env.end_scope();

            (translate::let_exp(var_init_irs, let_body_ir), let_body_ty)
        }
        Exp::ArrayExp {
            typ,
            size,
            init,
            pos,
        } => {
            let sym = ctx.intern(typ);
            let arr_ty_opt = ctx.type_env.look(sym).map(|e| e.clone());
            if arr_ty_opt.is_none() {
                ctx.flag_error_with_msg(&format!(
                    "Trying to use an undeclared array type {}",
                    ctx.get_span(typ)
                ));
                (ERROR_TR_EXP, Type::Error)
            } else {
                match arr_ty_opt.as_ref().unwrap() {
                    Type::Array(ele_ty, b) => {
                        let (init_val_ir, init_val_ty) =
                            trans_exp::<T>(ctx, level.clone(), init, break_label);
                        if !init_val_ty.compatible_with(&**ele_ty) {
                            ctx.flag_error_with_msg(&format!(
                                "array initialized with type {} but declared with type {}",
                                init_val_ty, ele_ty
                            ));
                            (ERROR_TR_EXP, Type::Error)
                        } else {
                            (translate::array_exp(), arr_ty_opt.unwrap().clone())
                        }
                    }
                    _ => {
                        ctx.flag_error_with_msg(&format!(
                            "{} is not of array type!",
                            ctx.get_span(typ)
                        ));
                        (ERROR_TR_EXP, Type::Error)
                    }
                }
            }
        }
    }
}

fn trans_var<T: Frame + 'static>(
    ctx: &mut TypeCheckingContext,
    level: Rc<RefCell<Level>>,
    var: &Var,
    break_label: Option<Label>,
) -> ExpTy {
    match var {
        Var::SimpleVar(span, pos) => {
            let sym = ctx.intern(span);
            let entry_opt = ctx.varfun_env.look(sym);
            match entry_opt {
                None => {
                    ctx.flag_error_with_msg(&format!(
                        "use of undeclared variable {}",
                        ctx.get_span(span)
                    ));
                    (ERROR_TR_EXP, Type::Error)
                }
                Some(EnvEntry::FunEntry { .. }) => {
                    // this version of tiger does not support using functions as lvalues.
                    // functions can only be called.
                    // therefore, this is an error.
                    ctx.flag_error_with_msg(&format!(
                        "attempting to use function {} as a lvalue",
                        ctx.get_span(span)
                    ));
                    (ERROR_TR_EXP, Type::Error)
                }
                Some(EnvEntry::VarEntry { ty, readonly, .. }) => match ty {
                    Type::Error => (ERROR_TR_EXP, Type::Error),
                    _ => (translate::simple_var(), ty.clone()),
                },
            }
        }
        Var::FieldVar(lhs_var, rhs_span, pos) => {
            let (lhs_var_ir, lhs_var_ty) = trans_var::<T>(ctx, level.clone(), lhs_var, break_label);
            match lhs_var_ty {
                Type::Record(field_types, ord) => {
                    let sym = ctx.intern(rhs_span);
                    let mut decl_ty = None;
                    let mut field_offset = 0;
                    for (field_sym, ty) in &*field_types {
                        if *field_sym == sym {
                            decl_ty = Some(ty);
                            break;
                        } else {
                            field_offset += 1;
                        }
                    }
                    match decl_ty {
                        None => {
                            ctx.flag_error_with_msg(&format!(
                                "{} is not a field of the record",
                                ctx.get_span(rhs_span)
                            ));
                            (ERROR_TR_EXP, Type::Error)
                        }
                        Some(dty) => {
                            // in our cute toy language every record field
                            //  is scalar or pointer so they have same
                            //  size, so we don't even have to do any
                            // extra work calculating the record size.
                            (
                                translate::record_field(lhs_var_ir, field_offset),
                                dty.clone(),
                            )
                        }
                    }
                }
                x => {
                    ctx.flag_error_with_msg(&format!(
                        "tried to access field of a non-record type {}",
                        x
                    ));
                    (ERROR_TR_EXP, Type::Error)
                }
            }
        }
        Var::SubscriptVar(deref_var, index_exp, pos) => {
            let (lhs_ir, lhs_ty) = trans_var::<T>(ctx, level.clone(), deref_var, break_label);
            match lhs_ty {
                Type::Array(ele_ty, ord) => {
                    let (idx_ir, idx_ty) =
                        trans_exp::<T>(ctx, level.clone(), index_exp, break_label);
                    match idx_ty {
                        Type::Int => {
                            let sym = ctx.symbols.intern("exit");
                            let exit_fn_entry = ctx.varfun_env.look(sym);
                            match exit_fn_entry {
                                None => {
                                    panic!("bug in impl, missing the built-in exit procedure");
                                }
                                Some(EnvEntry::FunEntry { label, .. }) => (
                                    translate::subscript_var(lhs_ir, idx_ir, *label),
                                    (**ele_ty).clone(),
                                ),
                                Some(x) => {
                                    panic!("bug in impl, `exit` should be a FunEntry but is {}", x);
                                }
                            }
                        }
                        _ => {
                            ctx.flag_error_with_msg("array index must be of Int type!");
                            (ERROR_TR_EXP, Type::Error)
                        }
                    }
                }
                Type::Error => (ERROR_TR_EXP, Type::Error),
                x => {
                    ctx.flag_error_with_msg(&format!(
                        "Subscript is only valid for array, used on {}",
                        x
                    ));
                    (ERROR_TR_EXP, Type::Error)
                }
            }
        }
    }
}

fn ty_to_type(ctx: &mut TypeCheckingContext, ty: &Ty) -> Type {
    match ty {
        Ty::NameTy(span, pos) => {
            let sym = ctx.intern(&span);
            match ctx.type_env.look(sym) {
                None => Type::Name(sym),
                Some(x) => x.clone(),
            }
        }
        Ty::ArrayTy(span, pos) => {
            let sym = ctx.intern(&span);
            match ctx.type_env.look(sym) {
                // This case happen when array refers to a type that was not encountered yet
                // but would expect to encounter later in a legal program.
                None => Type::Array(
                    Rc::new(Box::new(Type::Name(sym))),
                    ctx.get_next_array_record_ord(),
                ),
                // This case happen when array refers to a type that was encountered already.
                Some(x) => Type::Array(
                    Rc::new(Box::new(x.clone())),
                    ctx.get_next_array_record_ord(),
                ),
            }
        }
        Ty::RecordTy(fields) => {
            let mut seen = HashMap::new();
            let mut record_field_types = Vec::new();
            let mut err = false;

            for field in fields {
                // can't use get_span because it makes borrow checker think we have
                // immutable ref to ctx and we later call a mutation method on ctx,
                // but somehow accessing a field of ctx is okay. ¯_(ツ)_/¯
                let field_name = &ctx.input[field.name.start()..field.name.end()];
                if seen.contains_key(field_name) {
                    err = true;
                    ctx.flag_error_with_msg(&format!(
                        "field {} declared more than once in record.",
                        field_name
                    ));
                } else {
                    seen.insert(field_name, ());
                    let field_sym = ctx.intern(&field.name);
                    let typ_sym = ctx.intern(&field.typ);
                    let type_opt = ctx.type_env.look(typ_sym);
                    match type_opt {
                        None => {
                            record_field_types.push((field_sym, Type::Name(typ_sym)));
                        }
                        Some(typ) => {
                            record_field_types.push((field_sym, typ.clone()));
                        }
                    }
                }
            }
            if err {
                Type::Error
            } else {
                Type::Record(Rc::new(record_field_types), ctx.get_next_array_record_ord())
            }
        }
    }
}

fn resolve_name_type(ctx: &mut TypeCheckingContext, t: &Type) {
    fn resolve_and_update(s: Symbol, ctx: &mut TypeCheckingContext) {
        let resolved = ctx.type_env.look(s);
        if resolved.is_none() {
            ctx.flag_error_with_msg(&format!(
                "{} is undeclared type",
                ctx.symbols.resolve(&s).unwrap()
            ));
        } else {
            ctx.type_env.enter(s, resolved.unwrap().clone());
        }
    }
    // Mutates the type_env and updates the mapping for the Name types.
    match t {
        Type::Name(s) => {
            resolve_and_update(*s, ctx);
        }
        Type::Array(ty, _) => match **ty.as_ref() {
            Type::Name(s) => {
                resolve_and_update(s, ctx);
            }
            _ => {}
        },
        Type::Record(field_types, _) => {
            for (_, typ) in &**field_types {
                match typ {
                    Type::Name(s) => {
                        resolve_and_update(*s, ctx);
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }
}

fn trans_dec<T: Frame + 'static>(
    ctx: &mut TypeCheckingContext,
    level: Rc<RefCell<Level>>,
    dec: &Dec,
    break_label: Option<Label>,
) -> Option<TrExp> {
    match dec {
        Dec::FunctionDec(fundecs) => {
            // no two functions in a sequence of mutually recursive functions
            // may have the same name.
            // functions may be recursive. mutually recursive functions and procs
            // are declared by a seq of conseq func declarations with no intervening
            // type or var decls.
            let mut seen = HashMap::new();

            // first pass: add the fun entry
            for fundec in fundecs {
                let name = &ctx.input[fundec.name.start()..fundec.name.end()];
                if seen.contains_key(name) {
                    ctx.flag_error_with_msg(&format!("{} is declared more than once in a sequence of mutually recursive functions", name));
                } else {
                    seen.insert(name, ());
                }

                let ret_ty = if fundec.result.is_none() {
                    Type::Unit
                } else {
                    let (span, _) = fundec.result.unwrap();
                    let sym = ctx.intern(&span);
                    let result_type_opt = ctx.type_env.look(sym);
                    if result_type_opt.is_none() {
                        Type::Error
                    } else {
                        result_type_opt.unwrap().clone()
                    }
                };

                let mut formal_types = Vec::new();
                let mut escapes = Vec::new();
                for field in &fundec.params {
                    let name = ctx.intern(&field.name);
                    let field_type_opt = ctx.type_env.look(name);
                    match field_type_opt {
                        None => {
                            formal_types.push(Type::Error);
                        }
                        Some(ty) => {
                            formal_types.push(ty.clone());
                        }
                    }
                    escapes.push(field.escape);
                }
                let new_level = Level::new_level::<T>(
                    level.clone(),
                    escapes,
                    &mut ctx.gen_temp_label,
                    &mut ctx.symbols,
                );
                let funentry = EnvEntry::FunEntry {
                    level: new_level,
                    label: level.as_ref().borrow().get_label().unwrap(), // it is a bug if this does not have a label.
                    formals: Rc::new(formal_types),
                    result: ret_ty,
                };
                let sym = ctx.intern(&fundec.name);
                ctx.varfun_env.enter(sym, funentry);
            }

            // second pass
            for fundec in fundecs {
                let sym = ctx.intern(&fundec.name);
                ctx.varfun_env.begin_scope();

                let fun_entry = ctx.varfun_env.look(sym).map(|e| e.clone());

                match fun_entry {
                    None | Some(EnvEntry::VarEntry { .. }) => {
                        panic!(
                            "Bug in impl. Expected FunEntry, which should have been just interned."
                        );
                    }
                    Some(EnvEntry::FunEntry {
                        level,
                        label,
                        formals,
                        result,
                    }) => {
                        let formals_access = level.as_ref().borrow().formals();
                        for i in 0..fundec.params.len() {
                            let var_entry = EnvEntry::VarEntry {
                                ty: formals[i].clone(),
                                readonly: true,
                                access: formals_access[i].clone(),
                            };
                            let sym = ctx.intern(&fundec.params[i].name);
                            ctx.varfun_env.enter(sym, var_entry);
                        }

                        // the break label is not inherited in the function call
                        let (fun_body_ir, fun_body_ty) =
                            trans_exp::<T>(ctx, level.clone(), &*fundec.body, None);
                        if !fun_body_ty.compatible_with(&result)
                            && !matches!(Type::Error, fun_body_ty)
                            && !matches!(Type::Error, result)
                        {
                            ctx.flag_error_with_msg(&format!(
                                "expected return type {} for function but got {}",
                                result, fun_body_ty
                            ));
                        }
                        // TODO deal with the fragments crap when it comes to that
                        translate::proc_entry_exit(level.clone(), fun_body_ir);
                    }
                }
                ctx.varfun_env.end_scope();
            }
            None
        }
        Dec::VarDec {
            name,
            escape,
            typ,
            init,
            pos,
        } => {
            let sym = ctx.intern(name);
            let name = &ctx.input[name.start()..name.end()];
            let (init_ir, init_ty) = trans_exp::<T>(ctx, level.clone(), init, break_label);
            match typ {
                None => {
                    if matches!(Type::Nil, init_ty) {
                        ctx.flag_error_with_msg(&format!("Variable {} is declared with unknown type and initiated with nil. Fix by using the long form, e.g. var {} : <your-type> = ...", name, name));
                    } else {
                        // no return type specified, infer it
                        let acc = level.as_ref().borrow_mut().alloc_local(*escape);
                        ctx.varfun_env.enter(
                            sym,
                            EnvEntry::VarEntry {
                                ty: init_ty,
                                readonly: false,
                                access: acc,
                            },
                        );
                    }
                    None
                }
                Some((span, pos)) => {
                    let sym = ctx.intern(span);
                    match ctx.type_env.look(sym) {
                        None => {
                            ctx.flag_error_with_msg(&format!(
                                "unknown return type {}",
                                ctx.get_span(span)
                            ));
                            ctx.varfun_env.enter(
                                sym,
                                EnvEntry::VarEntry {
                                    ty: Type::Error,
                                    readonly: true,
                                    access: (level.clone(), frame::Access::InFrame(42)),
                                },
                            );
                        }
                        Some(t) => {
                            if !t.compatible_with(&init_ty) {
                                ctx.flag_error_with_msg(&format!("return type of {} does not match actual type {} of initializer.", init_ty, t));
                                ctx.varfun_env.enter(
                                    sym,
                                    EnvEntry::VarEntry {
                                        ty: Type::Error,
                                        readonly: true,
                                        access: (level.clone(), frame::Access::InFrame(42)),
                                    },
                                );
                            } else {
                                let acc = level.as_ref().borrow_mut().alloc_local(*escape);
                                ctx.varfun_env.enter(
                                    sym,
                                    EnvEntry::VarEntry {
                                        ty: init_ty,
                                        readonly: false,
                                        access: acc,
                                    },
                                );
                            }
                        }
                    }
                    None
                }
            }
        }

        Dec::TypeDec(tydecs) => {
            // spec from appendix:
            // mutually recursive types are declared by a conseq seq of type dec
            // without intervening value or func dec. Each recursion cycle must
            // pass through a record or array type.
            // ...
            // no two types in a seq of mutually recursive types may have the
            // same name.
            //

            let mut seen = HashMap::new();
            let mut to_fix_up = Vec::new();

            for dec in tydecs {
                let name = &ctx.input[dec.name.start()..dec.name.end()];
                let sym = ctx.intern(&dec.name);
                if seen.contains_key(&sym) {
                    ctx.flag_error_with_msg(&format!(
                        "{} is declared more than once in a sequence of mutually recursive types.",
                        name
                    ));
                    ctx.type_env.enter(sym, Type::Error);
                } else {
                    seen.insert(sym, ());
                    let ty = ty_to_type(ctx, &*dec.ty);
                    ctx.type_env.enter(sym, ty.clone());
                    to_fix_up.push((ty, dec.pos));
                }
            }
            // Cycle detection. A cycle happens if we can follow 1 or more Type::Name to an existing type.
            for dec in tydecs {
                let sym = ctx.intern(&dec.name);
                let name = ctx.get_span(&dec.name);
                seen.clear(); // reuse
                let mut ty = ctx.type_env.look(sym).unwrap(); // safe because we updated type_env in above loop.
                seen.insert(sym, ());
                while let Type::Name(s) = ty {
                    if seen.contains_key(s) {
                        ctx.flag_error_with_msg(&format!(
                            "circular type definition detected for type {}",
                            name
                        ));
                        break;
                    }
                    seen.insert(*s, ());
                    ty = ctx.type_env.look(*s).unwrap();
                }
            }
            // Fix up the thing by eliminating Type::Name
            for (to_fix, pos) in to_fix_up {
                resolve_name_type(ctx, &to_fix);
            }
            None
        }
    }
}

pub fn translate<T: Frame + 'static>(input: &str, ast: &Exp) -> Result<TrExp, ()> {
    let mut ctx = TypeCheckingContext::new(input);

    let main_level = Level::new_level::<T>(
        Level::outermost(),
        Vec::new(),
        &mut ctx.gen_temp_label,
        &mut ctx.symbols,
    );
    let (exp, ty) = trans_exp::<T>(&mut ctx, main_level, ast, None);
    match ty {
        Type::Error => Err(()),
        _ => Ok(exp),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        frame,
        frame::{Escapes, Frame},
        symbol::Interner,
    };
    struct TestFrame;

    impl Frame for TestFrame {
        fn new(name: Label, formals: Vec<Escapes>) -> Self {
            TestFrame
        }
        fn name(&self) -> Label {
            todo!()
        }
        fn formals(&self) -> Vec<frame::Access> {
            todo!()
        }
        fn alloc_local(&mut self, escapes: Escapes) -> frame::Access {
            todo!()
        }
    }

    #[test]
    fn todotodo() {}
}

// proc newFrame*(x: typedesc[TestFrame], name: Label, formals: seq[Escape]): TestFrame =
//     result.name = name
//     for escape in formals:
//         if escape:
//             result.formals.add frame.Access(kind: InFrame, offset: 42)
//         else:
//             result.formals.add frame.Access(kind: InReg, reg: newTemp())

// proc name*(f: TestFrame): Label =
//     return f.name

// proc formals*(f: TestFrame): seq[frame.Access] =
//     return f.formals

// proc allocLocal*(vf: var TestFrame, escapes: Escape): frame.Access =
//     frame.Access(kind:InReg, reg: newTemp())

// proc externalCall*(x: typedesc[TestFrame], y: string, z: seq[IrExp]) : IrExp =
//     Const(42)

// proc FP*(x: typedesc[TestFrame]) : temp.Temp =
//     return temp.newTemp()

// proc wordSize*(x: typedesc[TestFrame]) : int =
//     return 42

// proc exp*(x: typedesc[TestFrame], a: frame.Access, ir: IrExp) : IrExp =
//     Const(42)

// proc procEntryExit1*(f: TestFrame, s: IrStm) : IrStm = s

// test "sanity check TestFrame is Frame":
//     doAssert TestFrame is Frame

// proc testInput(input: string, expectGood: bool) =
//     let astOpt = parseString(input)
//     doAssert astOpt.isSome
//     let texpOpt = transProg[TestFrame](astOpt.get)
//     if expectGood:
//         doAssert texpOpt.isSome
//     else:
//         doAssert texpOpt.isNone

// proc testInputIsGood(input: string) =
//     testInput input, true

// proc testInputIsBad(input: string) =
//     testInput input, false

// proc testBad(input: string) =
//     let astOpt = parseString(input)
//     doAssert astOpt.isSome
//     let texpOpt = transProg[TestFrame](astOpt.get)
//     doAssert texpOpt.isNone

// proc testFile(f: string, expectGood: bool) =
//     echo "\ntesting with input: ", f
//     var input = readFile(f)
//     let astOpt = parseString(input)
//     doAssert astOpt.isSome
//     let texpOpt = transProg[TestFrame](astOpt.get)
//     if expectGood:
//         doAssert texpOpt.isSome, f
//     else:
//         doAssert texpOpt.isNone, f

// test "appel tiger test programs good":
//     for f in walkFiles("tests/tiger_test_programs/semant/good/*"):
//         testFile f, true

// test "appel tiger test programs bad":
//     for f in walkFiles("tests/tiger_test_programs/semant/bad/*"):
//         testFile f, false

// test "Type ==":
//     var x, y: semant.Type
//     x = Type(kind: ErrorT)
//     y = Type(kind: ErrorT)
//     check x == y

// test "$ Symtab[Type]":
//     var st = newSymtab[semant.Type]()
//     st.beginScope()
//     for i in 0..100:
//         st.enter symbol $i, semant.Type(kind: ErrorT)
//     discard $st

// test "$ Symtab[EnvEntry]":
//     var st = newSymtab[EnvEntry[TestFrame]]()
//     st.beginScope()
//     for i in 0..100:
//         st.enter symbol $i, EnvEntry[TestFrame](kind: VarEntry, ty: Type(kind: ErrorT))
//     discard $st

// test "var a := nil fails type check":
//     let source = """let
//     var a := nil
//     in 1
//     end"""
//     testInputIsBad source

// test "nil valid cases in appendix":
//     let source = """let
//     type any = {any: int}
//     var a : any := nil
//     function f(a: any) : int = 100
//     in
//     a := nil;
//     if a <> nil then 1 else 1;
//     if nil <> a then 2 else 2;
//     if a = nil then 3 else 3;
//     if nil = a then 4 else 4;
//     f(nil)
//     end"""
//     testInputIsGood source

// test "record a{f1=nil,...} works":
//     let source = """let
//     type any = {any: int, x: any}
//     var a : any := any{any=1, x=nil}
//     in
//     42
//     end"""
//     testInputIsGood source

// test "if...then... can both return nil":
//     let source = """let
//     type any = {any: int}
//     in
//     if 1 = 1 then nil else nil
//     end"""
//     testInputIsGood source

// test "if...then..., if returns RecordT, then returns nil":
//     let source = """let
//     type any = {any: int}
//     var a: any := any{any=1}
//     in
//     if 1 = 1 then a else nil
//     end"""
//     testInputIsGood source

// test "if...then..., if returns nil, then returns RecordT":
//     let source = """let
//     type any = {any: int}
//     var a: any := any{any=1}
//     in
//     if 1 = 1 then nil else a
//     end"""
//     testInputIsGood source

// test "function a() : some_record = ... where body returns nil":
//     let source = """let
//     type any = {any: int}
//     function f(a: any) : any = nil
//     in
//     f(nil)
//     end"""
//     testInputIsGood source

// test "function a() = () should be accepted":
//     let source = """let
//     function f() = ()
//     in
//     f()
//     end"""
//     testInputIsGood source

// test "for loop counter cannot be assigned to":
//     let source = """let
//     in
//     for r := 0 to 10 do
//         if 1 = 1 then r := 1; ()
//     end"""
//     testInputIsBad source

// test "records same but separate decls are distinct types":
//     let source = """let
//     type any1 = {any: int}
//     type any2 = {any: int}
//     var a : any1 := nil
//     var b : any2 := nil
//     in
//     a = b
//     end"""
//     testInputIsBad source

// test "arrays same but separate decls are distinct types":
//     let source = """let
//     type arr1 = array of int
//     type arr2 = array of int
//     var a1 := arr1[8] of 0
//     var a2 := arr2[8] of 0
//     in
//     a1 = a2
//     end"""
//     testInputIsBad source

// test "local redeclarations, appendix example":
//     let source = """
//     let
//         function print(v:int) = ()
//         function f(v:int) =
//         let var v := 6
//             in print(v);
//             let var v := 7 in print (v) end;
//             print(v);
//             let var v := 8 in print (v) end;
//             print (v)
//         end
//     in
//     ()
//     end"""
//     testInputIsGood source

// test "break not in while/for fails":
//     let source = """
//     let
//     in
//         break
//     end"""
//     testInputIsBad source

// test "break in a while loop is legal":
//     let source = """
//     let
//     in
//         while 1 = 1 do
//             break
//     end"""
//     testInputIsGood source

// test "break in a for loop is legal":
//     let source = """
//     let
//     in
//         for i :=0 to 100 do
//             break
//     end"""
//     testInputIsGood source

// test "standard library calls":
//     let source = """
//     let
//     in
//         print("die");
//         flush();
//         getchar();
//         ord("");
//         chr(0);
//         size("");
//         substring("hello", 0, 1);
//         concat("h","i");
//         not(1);
//         exit(1)
//     end"""
//     testInputIsGood source

// test "circular to self bad":
//     let source = """
//     let
//         type a = a
//     in
//     end"""
//     testInputIsBad source

// test "circular through seq of type decls bad":
//     let source = """
//     let
//         type a = b
//         type b = a
//     in
//     end"""
//     testInputIsBad source

// test "circular self through array okay":
//     let source = """
//     let
//         type a = array of a
//     in
//     end"""
//     testInputIsGood source

// test "circular self through record okay":
//     let source = """
//     let
//         type a = {x: a}
//     in
//     end"""
//     testInputIsGood source

// test "circular nonself through array ok":
//     let source = """
//     let
//         type a = b
//         type b = array of a
//     in
//     end"""
//     testInputIsGood source

// test "circular nonself through record ok":
//     let source = """
//     let
//         type a = b
//         type b = {x: a}
//     in
//     end"""
//     testInputIsGood source

// test "redeclaration":
//     let source = """
//     let
//         var i := 1
//         var i := 2 /* i above irrelevant for escape analysis */
//         var k := 3
//         /* function i's scope effectively at start of recursive blk */
//         function j () =
//             (i();
//             k + 1;
//             ())
//         function i () =
//             (2;
//             ())
//         var i := 4
//         function j () : int =
//             i + 1  /* i escapes here */
//     in
//     end
//     """
//     testInputIsGood source

// test "fundec block shadow var name":
//     let source = """
//     let
//         var i := 1
//         function j () =
//             i := 2 /* this is illegal because i is a func */
//         function i () =  /* i defined as func at start of j */
//             ()
//     in
//     end
//     """
//     testInputIsBad source

// test "escape simple":
//     let source = """
//     let
//         var i := 1
//         function j () =
//             i := 2
//     in
//         let
//             function k() =
//                 i := 3
//         in
//         end
//     end
//     """
//     let astOpt = parseString(source)
//     doAssert astOpt.isSome
//     let ast = astOpt.get
//     check ast.decs[0].escape == false
//     ast.findEscape
//     check ast.decs[0].escape == true
//     testInputIsGood source

// test "escape var get redeclared as function":
//     let source = """
//     let
//         var i := 1
//         function j () =
//             i()
//         function i () =
//             ()
//     in
//     end
//     """
//     let astOpt = parseString(source)
//     doAssert astOpt.isSome
//     let ast = astOpt.get
//     check ast.decs[0].escape == false
//     ast.findEscape
//     check ast.decs[0].escape == false
//     testInputIsGood source

// test "escape var gets redeclared":
//     let source = """
//     let
//         var i := 1 /* get shadowed so shouldn't move */
//         var i := 2 /* i escapes */
//         function j () =
//             i := 3
//         var i := 4 /* this is fresh and shouldn't escape */
//         var j := 5  /* gets used later */
//     in
//         let
//             function x() =
//                 j := 42
//         in
//         end
//     end
//     """
//     let astOpt = parseString(source)
//     doAssert astOpt.isSome
//     let ast = astOpt.get
//     check ast.decs[0].escape == false
//     check ast.decs[1].escape == false
//     check ast.decs[3].escape == false
//     check ast.decs[4].escape == false
//     ast.findEscape
//     check ast.decs[0].escape == false
//     check ast.decs[1].escape == true
//     check ast.decs[3].escape == false
//     check ast.decs[4].escape == true
//     testInputIsGood source

// test "for variable escapes":
//     let source = """
//     let
//         function x() =
//             for i := 1 to 100
//             do
//                 let
//                     function j () : int =
//                         i + 1
//                     var i := 2 /* redeclare */
//                     var k := 0
//                     function j () : int =
//                         i + 1  /* this now affects the inner i */
//                 in
//                 end
//     in
//     end
//     """
//     let astOpt = parseString(source)
//     doAssert astOpt.isSome
//     let ast = astOpt.get
//     check ast.decs[0].fundecs[0].body.escape == false
//     check ast.decs[0].fundecs[0].body.fbody.decs[1].escape == false # inner i
//     check ast.decs[0].fundecs[0].body.fbody.decs[2].escape == false # k
//     ast.findEscape
//     check ast.decs[0].fundecs[0].body.escape == true
//     check ast.decs[0].fundecs[0].body.fbody.decs[1].escape == true # inner i
//     check ast.decs[0].fundecs[0].body.fbody.decs[2].escape == false # k
//     testInputIsGood source

// test "for lo, hi does not affect escape of for loop var":
//     let source = """
//     let
//         var i:= 1
//         function x() =
//             for i := let function j() = i:= 1 in 1 end to let function j() = i:= 1 in 100 end
//             do
//                 ()
//     in
//     end
//     """
//     let astOpt = parseString(source)
//     doAssert astOpt.isSome
//     let ast = astOpt.get
//     check ast.decs[0].escape == false # outer i
//     check ast.decs[1].fundecs[0].body.escape == false # for loop i
//     ast.findEscape
//     check ast.decs[0].escape == true # outer i
//     check ast.decs[1].fundecs[0].body.escape == false # for loop i
//     testInputIsGood source

// test "for loop bad input to access for loop var in lo or hi":
//     let source = """
//     let
//         function x() =
//             for i :=  let function j() = i:= 1 in 1 end to let function j() = i:= 100 in 100 end
//             do
//                 ()
//     in
//     end
//     """
//     testInputIsBad source

// test "assignment to function call scalar result is invalid":
//     ## spec says lvalue is variables, proc params, fields of records, and
//     ## array elements. functions can return record or array type, but those
//     ## would be references to some block of memory, and assigning to that
//     ## reference doesn't make the same sense as assigning to a var of
//     ## array/record type. for other types, you are just returning a value,
//     ## so it makes no sense to assign to those either.
//     ##
//     ## luckily, it's built into the syntax so we don't have to worry about
//     ## that scenario.
//     let source = """
//     let
//         function x() : int =
//             100
//     in
//         x() := 20
//     end
//     """
//     let astOpt = parseString(source)
//     # it doesn't let you assign to func call results.
//     check astOpt.isNone

// test "assignment to function call array result is invalid":
//     let source = """
//     let
//         type intArray = array of int
//         function x() : intArray =
//             intArray[8] of 0
//         var t := intArray[8] of 0
//     in
//         x() := t
//     end
//     """
//     let astOpt = parseString(source)
//     # it doesn't let you assign to func call results.
//     check astOpt.isNone

// test "assignment to function call record result is invalid":
//     let source = """
//     let
//         type rec = {i : int}
//         function x() : rec =
//             rec{i=42}
//         var t := rec{i=43}
//     in
//         x() := t
//     end
//     """
//     let astOpt = parseString(source)
//     # it doesn't let you assign to func call results.
//     check astOpt.isNone

// test "functions returning array okay":
//     let source = """
//     let
//             type intArray = array of int
//             function x() : intArray =
//                 intArray[7] of 9
//     in
//     end
//     """
//     testInputIsGood source

// test "function returning record okay":
//     let source = """
//     let
//             type rec = {i: int}
//             function x() : rec =
//                 rec{i=42}
//     in
//     end
//     """
//     testInputIsGood source
