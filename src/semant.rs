use cfgrammar::Span;
use std::rc::Rc;

use crate::{
    absyn::{Dec, Exp, Field, Fundec, Oper, Ty, TyDec, Var},
    symbol::{Interner, Symbol},
    symtab::SymbolTable,
    temp::{Label, LabelAuthority, Temp, TempAuthority},
    translate,
    translate::{TrExp, ERROR_TR_EXP},
};
use strum_macros::Display;

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct ArrayTypeOrdinal(usize);

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct RecordTypeOrdinal(usize);

pub struct TypeCheckingContext<'a> {
    next_array_ord: usize,
    next_record_ord: usize,
    type_env: SymbolTable<Type>,
    varfun_env: SymbolTable<EnvEntry>,
    has_err: bool,
    symbols: Interner,
    input: &'a str,
    ta: TempAuthority,
    la: LabelAuthority,
}

impl<'a> TypeCheckingContext<'a> {
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

    fn intern(&self, s: &Span) -> Symbol {
        self.symbols.intern(self.get_span(s))
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
    Record(Rc<Vec<(Symbol, Type)>>, RecordTypeOrdinal),
    Nil,
    Int,
    String,
    Array(Rc<Box<Type>>, ArrayTypeOrdinal),
    Unit,
    Name,
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

enum EnvEntry {
    VarEntry { ty: Type, readonly: bool },
    FunEntry { formals: Vec<Type>, result: Type },
}

fn base_env_type_env(pool: &mut Interner) -> SymbolTable<Type> {
    let mut res = SymbolTable::empty();
    res.begin_scope();
    res.enter(pool.intern("int"), Type::Int);
    res.enter(pool.intern("string"), Type::String);
    res
}

fn base_varfun_env(pool: &mut Interner) -> SymbolTable<EnvEntry> {
    let mut res = SymbolTable::empty();
    res.begin_scope();
    res.enter(
        pool.intern("print"),
        EnvEntry::FunEntry {
            formals: vec![Type::String],
            result: Type::String,
        },
    );
    res.enter(
        pool.intern("flush"),
        EnvEntry::FunEntry {
            formals: vec![],
            result: Type::Unit,
        },
    );
    res.enter(
        pool.intern("getchar"),
        EnvEntry::FunEntry {
            formals: vec![],
            result: Type::String,
        },
    );
    res.enter(
        pool.intern("ord"),
        EnvEntry::FunEntry {
            formals: vec![Type::String],
            result: Type::Int,
        },
    );
    res.enter(
        pool.intern("chr"),
        EnvEntry::FunEntry {
            formals: vec![Type::Int],
            result: Type::String,
        },
    );
    res.enter(
        pool.intern("size"),
        EnvEntry::FunEntry {
            formals: vec![Type::String],
            result: Type::Int,
        },
    );
    res.enter(
        pool.intern("substring"),
        EnvEntry::FunEntry {
            formals: vec![Type::String, Type::Int, Type::Int],
            result: Type::String,
        },
    );
    res.enter(
        pool.intern("concat"),
        EnvEntry::FunEntry {
            formals: vec![Type::String, Type::String],
            result: Type::String,
        },
    );
    res.enter(
        pool.intern("not"),
        EnvEntry::FunEntry {
            formals: vec![Type::Int],
            result: Type::Int,
        },
    );
    res.enter(
        pool.intern("exit"),
        EnvEntry::FunEntry {
            formals: vec![Type::Int],
            result: Type::Unit,
        },
    );
    res
}

fn trans_exp(ctx: &mut TypeCheckingContext, n: &Exp, break_label: Option<Label>) -> ExpTy {
    match n {
        Exp::OpExp {
            left,
            right,
            oper,
            pos,
        } => {
            let (lhs_ir, lhs_ty) = trans_exp(ctx, left, break_label);
            let (rhs_ir, rhs_ty) = trans_exp(ctx, right, break_label);

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
        Exp::VarExp(v) => trans_var(ctx, v, break_label),
        Exp::StringExp(s, pos) => (translate::string_exp(ctx.get_span(s)), Type::String),
        Exp::CallExp { func, args, pos } => {
            let fentry_opt = ctx.varfun_env.look(ctx.intern(func));
            if fentry_opt.is_none() {
                ctx.flag_error_with_msg(&format!(
                    "Trying to call undeclared function {}",
                    ctx.get_span(func)
                ));
                return (ERROR_TR_EXP, Type::Error);
            } else if !matches!(fentry_opt.unwrap(), EnvEntry::FunEntry { .. }) {
                ctx.flag_error_with_msg(&format!("{} is not a function!", ctx.get_span(func)));
                return (ERROR_TR_EXP, Type::Error);
            } else {
                let Some(EnvEntry::FunEntry { formals, result }) = fentry_opt.unwrap();
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
                        let (arg_ir, arg_ty) = trans_exp(ctx, args.get(i).unwrap(), None);
                        if matches!(Type::Error, arg_ty)
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
                        (translate::call_exp(), *result)
                    }
                }
            }
        }
        Exp::RecordExp { fields, typ, pos } => {
            let rec_entry_opt = ctx.type_env.look(ctx.intern(typ));
            if rec_entry_opt.is_none() {
                ctx.flag_error_with_msg(&format!("{} has not been declared.", ctx.get_span(typ)));
                (ERROR_TR_EXP, Type::Error)
            } else if !matches!(rec_entry_opt.unwrap(), Type::Record(..)) {
                ctx.flag_error_with_msg(&format!("{} is not a record type.", ctx.get_span(typ)));
                (ERROR_TR_EXP, Type::Error)
            } else {
                let Type::Record(name_types, ord) = rec_entry_opt.unwrap();
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
                                let (site_ir, site_ty) = trans_exp(ctx, site_typ_exp, break_label);
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
                            Type::Record(name_types.clone(), *ord),
                        )
                    }
                }
            }
        }
        Exp::SeqExp(exps) => {
            let mut ret_val = Type::Unit;
            let exp_irs = Vec::new();
            for exp in exps {
                let (exp_ir, exp_ty) = trans_exp(ctx, exp, break_label);
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
            let (dst_ir, dst_ty) = trans_var(ctx, var, break_label);
            let (src_ir, src_ty) = trans_exp(ctx, exp, break_label);
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
                        let var_entry = ctx.varfun_env.look(ctx.intern(span));
                        assert!(var_entry.is_some(), "Impl bug: missing var entry");
                        let EnvEntry::VarEntry { ty, readonly } = var_entry.unwrap();

                        if *readonly {
                            ctx.flag_error_with_msg("Cannot assign to readonly location");
                            (ERROR_TR_EXP, Type::Error)
                        } else if matches!(ty, Type::Error) {
                            (ERROR_TR_EXP, Type::Error)
                        } else {
                            (translate::assignment(dst_ir, src_ir), Type::Unit)
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
            let (cond_ir, test_ty) = trans_exp(ctx, test.as_ref(), break_label);
            if !matches!(test_ty, Type::Int) {
                ctx.flag_error_with_msg(&format!(
                    "Conditionals must evaluate to INT but got {} here",
                    test_ty
                ));
                (ERROR_TR_EXP, Type::Error)
            } else {
                let (then_ir, then_ty) = trans_exp(ctx, then.as_ref(), break_label);
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
                    let (else_ir, else_ty) = trans_exp(ctx, els.unwrap().as_ref(), break_label);
                    if !else_ty.compatible_with(&then_ty) {
                        ctx.flag_error_with_msg(&format!("if-then-else branches have incompatible types: then has type {}, else has type {}", then_ty, else_ty));
                        (ERROR_TR_EXP, Type::Error)
                    } else {
                        (translate::conditional(cond_ir, then_ir, Some(else_ir)), then_ty)
                    }
                }
            }
        }
        Exp::WhileExp { test, body, pos } => {
            let while_done_label = Some(ctx.la.new_label(&mut ctx.symbols));
            // break is legal in expressions. so if they try to break in the condition
            // of a while loop, it is interpreted as breaking to the end of this while loop,
            // since logically, the while condition is re-evaluated each iteration.
            let (cond_ir, cond_ty) = trans_exp(ctx, test, while_done_label);
            if !matches!(cond_ty, Type::Int) {
                ctx.flag_error_with_msg("while condition must be of Int");
                (ERROR_TR_EXP, Type::Error)
            } else {
                let (body_ir, body_ty) = trans_exp(ctx, body, while_done_label);
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
            let for_done_label = Some(ctx.la.new_label(&mut ctx.symbols));
            // if "break" happens in evaluating the for loop params, will just break the loop itself.
            let (lo_ir, lo_ty) = trans_exp(ctx, lo, for_done_label);
            let (hi_ir, hi_ty) = trans_exp(ctx, hi, for_done_label);
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

            // TODO Level stuff, refer to nim code
            ctx.varfun_env.enter(
                ctx.intern(var),
                EnvEntry::VarEntry {
                    ty: Type::Int,
                    readonly: true,
                },
            );
            let (body_ir, body_ty) = trans_exp(ctx, body, for_done_label);
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

                if let Some(var_init_ir) = trans_dec(ctx, dec) {
                    var_init_irs.push(var_init_ir);
                }
            }
            let (let_body_ir, let_body_ty) = trans_exp(ctx, body, break_label);

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
            let arr_ty_opt = ctx.type_env.look(ctx.intern(typ));
            if arr_ty_opt.is_none() {
                ctx.flag_error_with_msg(&format!(
                    "Trying to use an undeclared array type {}",
                    ctx.get_span(typ)
                ));
                return (ERROR_TR_EXP, Type::Error)
            } else {
                match arr_ty_opt.unwrap() {
                    Type::Array(ele_ty, b) => {
                        let (init_val_ir, init_val_ty) = trans_exp(ctx, init, break_label);
                        if !init_val_ty.compatible_with(ele_ty) {
                            ctx.flag_error_with_msg(&format!(
                                "array initialized with type {} but declared with type {}",
                                init_val_ty, ele_ty
                            ));
                            return (ERROR_TR_EXP, Type::Error)
                        } else {
                            return (translate::array_exp(), *arr_ty_opt.unwrap())
                        }
                    }
                    _ => {
                        ctx.flag_error_with_msg(&format!(
                            "{} is not of array type!",
                            ctx.get_span(typ)
                        ));
                        return (ERROR_TR_EXP, Type::Error)
                    }
                }
            }
        }
    }
}

fn trans_var(ctx: &mut TypeCheckingContext, var: &Var, break_label: Option<Label>) -> ExpTy {
    todo!()
}

fn trans_dec(ctx: &mut TypeCheckingContext, dec: &Dec) -> Option<TrExp> {
    todo!()
}

fn translate(input: &str, ast: &Exp) -> Result<TrExp, ()> {
    let mut ctx = TypeCheckingContext {
        next_array_ord: 0,
        next_record_ord: 0,
        type_env: SymbolTable::empty(),
        varfun_env: SymbolTable::empty(),
        has_err: false,
        symbols: Interner::new(),
        input: input,
        ta: TempAuthority::new(),
        la: LabelAuthority::new(),
    };

    let (exp, ty) = trans_exp(&mut ctx, ast, None);
    match ty {
        Type::Error => Err(()),
        _ => Ok(exp),
    }
}
