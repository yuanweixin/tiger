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
    fn new(input: &'a str) -> Self {
        let ta = TempAuthority::new();
        let mut la = LabelAuthority::new();
        let mut symbols = Interner::new();
        let type_env = Self::base_env_type_env(&mut symbols);
        let varfun_env = Self::base_varfun_env(&mut symbols, &mut la);
        Self {
            next_array_ord: 0,
            next_record_ord: 0,
            type_env: type_env,
            varfun_env: varfun_env,
            has_err: false,
            symbols: symbols,
            input: input,
            ta: ta,
            la: la
        }
    }


fn base_env_type_env(pool: &mut Interner) -> SymbolTable<Type> {
    let mut res = SymbolTable::empty();
    res.begin_scope();
    res.enter(pool.intern("int"), Type::Int);
    res.enter(pool.intern("string"), Type::String);
    res
}

fn base_varfun_env(pool: &mut Interner, la: &mut LabelAuthority) -> SymbolTable<EnvEntry> {
    let mut res = SymbolTable::empty();
    res.begin_scope();
    res.enter(
        pool.intern("print"),
        EnvEntry::FunEntry {
            formals: Rc::new(vec![Type::String]),
            result: Type::String,
            label: la.new_label(pool)
        },
    );
    res.enter(
        pool.intern("flush"),
        EnvEntry::FunEntry {
            formals: Rc::new(vec![]),
            result: Type::Unit,
            label: la.new_label(pool)
        },
    );
    res.enter(
        pool.intern("getchar"),
        EnvEntry::FunEntry {
            formals: Rc::new(vec![]),
            result: Type::String,
            label: la.new_label(pool)
        },
    );
    res.enter(
        pool.intern("ord"),
        EnvEntry::FunEntry {
            formals: Rc::new(vec![Type::String]),
            result: Type::Int,
            label: la.new_label(pool)
        },
    );
    res.enter(
        pool.intern("chr"),
        EnvEntry::FunEntry {
            formals: Rc::new(vec![Type::Int]),
            result: Type::String,
            label: la.new_label(pool)
        },
    );
    res.enter(
        pool.intern("size"),
        EnvEntry::FunEntry {
            formals: Rc::new(vec![Type::String]),
            result: Type::Int,
            label: la.new_label(pool)
        },
    );
    res.enter(
        pool.intern("substring"),
        EnvEntry::FunEntry {
            formals: Rc::new(vec![Type::String, Type::Int, Type::Int]),
            result: Type::String,
            label: la.new_label(pool)
        },
    );
    res.enter(
        pool.intern("concat"),
        EnvEntry::FunEntry {
            formals: Rc::new(vec![Type::String, Type::String]),
            result: Type::String,
            label: la.new_label(pool)
        },
    );
    res.enter(
        pool.intern("not"),
        EnvEntry::FunEntry {
            formals: Rc::new(vec![Type::Int]),
            result: Type::Int,
            label: la.new_label(pool)
        },
    );
    res.enter(
        pool.intern("exit"),
        EnvEntry::FunEntry {
            formals: Rc::new(vec![Type::Int]),
            result: Type::Unit,
            label: la.new_label(pool)
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

#[derive(Clone, Display)]
enum EnvEntry {
    VarEntry {
        ty: Type,
        readonly: bool,
    },
    FunEntry {
        label: Label,
        formals: Rc<Vec<Type>>,
        result: Type,
    },
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
                    formals,
                    result,
                    label,
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
                            let (arg_ir, arg_ty) = trans_exp(ctx, args.get(i).unwrap(), None);
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
                                    let (site_ir, site_ty) =
                                        trans_exp(ctx, site_typ_exp, break_label);
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
                            Some(EnvEntry::VarEntry { ty, readonly }) => {
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
                    let (else_ir, else_ty) = trans_exp(ctx, els.as_ref().unwrap(), break_label);
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

            let sym = ctx.intern(var);
            // TODO Level stuff, refer to nim code
            ctx.varfun_env.enter(
                sym,
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
                        let (init_val_ir, init_val_ty) = trans_exp(ctx, init, break_label);
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

fn trans_var(ctx: &mut TypeCheckingContext, var: &Var, break_label: Option<Label>) -> ExpTy {
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
                Some(EnvEntry::VarEntry { ty, readonly }) => match ty {
                    Type::Error => (ERROR_TR_EXP, Type::Error),
                    _ => (translate::simple_var(), ty.clone()),
                },
            }
        }
        Var::FieldVar(lhs_var, rhs_span, pos) => {
            let (lhs_var_ir, lhs_var_ty) = trans_var(ctx, lhs_var, break_label);
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
                            (translate::record_field(lhs_var_ir, field_offset), dty.clone())
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
            let (lhs_ir, lhs_ty) = trans_var(ctx, deref_var, break_label);
            match lhs_ty {
                Type::Array(ele_ty, ord) => {
                    let (idx_ir, idx_ty) = trans_exp(ctx, index_exp, break_label);
                    match idx_ty {
                        Type::Int => {
                            let sym = ctx.symbols.intern("exit");
                            let exit_fn_entry = ctx.varfun_env.look(sym);
                            match exit_fn_entry {
                                None => {
                                    panic!("bug in impl, missing the built-in exit procedure");
                                }
                                Some(EnvEntry::FunEntry { formals, result , label}) => {
                                    (
                                        translate::subscript_var(lhs_ir, idx_ir, *label),
                                        (**ele_ty).clone(),
                                    )
                                }
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

fn trans_dec(ctx: &mut TypeCheckingContext, dec: &Dec) -> Option<TrExp> {
    match dec {
        Dec::FunctionDec(fundecs) => {}
        Dec::VarDec {
            name,
            escape,
            typ,
            init,
            pos,
        } => {}

        Dec::TypeDec(tydecs) => {}
    }
    todo!()
}

pub fn translate(input: &str, ast: &Exp) -> Result<TrExp, ()> {
    let mut ctx = TypeCheckingContext::new(input);

    let (exp, ty) = trans_exp(&mut ctx, ast, None);
    match ty {
        Type::Error => Err(()),
        _ => Ok(exp),
    }
}
