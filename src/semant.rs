use cfgrammar::Span;

use crate::{
    absyn::{Dec, Exp, Field, Fundec, Oper, Ty, TyDec, Var},
    symbol::{Interner, Symbol},
    symtab::SymbolTable,
    temp::Label,
    translate,
    translate::{TrExp, ERROR_TR_EXP},

};
use strum_macros::Display;

#[derive(Eq, PartialEq)]
pub struct ArrayTypeOrdinal(usize);

#[derive(Eq, PartialEq)]
pub struct RecordTypeOrdinal(usize);

pub struct TypeCheckingContext<'a> {
    next_array_ord: usize,
    next_record_ord: usize,
    type_env: SymbolTable<Type>,
    varfun_env: SymbolTable<EnvEntry>,
    has_err: bool,
    symbols: Interner,
    input: &'a str,
}

impl<'a> TypeCheckingContext<'a> {
    fn flag_error_with_msg(&mut self, msg: String) {
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
}

#[derive(Eq, PartialEq, Display)]
pub enum Type {
    Record(Vec<(Symbol, Box<Type>)>, RecordTypeOrdinal),
    Nil,
    Int,
    String,
    Array(Box<Type>, ArrayTypeOrdinal),
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
    VarEntry { ty: Type },
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
                        (Type::Int, Type::Int) => (translate::binop(oper, lhs_ir, rhs_ir), Type::Int),
                        (Type::Error, _) | (_, Type::Error) => (ERROR_TR_EXP, Type::Error),
                        (Type::Int, _) => {
                            ctx.flag_error_with_msg(format!(
                                "Expected integer on rhs but got {}",
                                right
                            ));
                            (ERROR_TR_EXP, Type::Error)
                        }
                        (_, Type::Int) => {
                            ctx.flag_error_with_msg(format!(
                                "Expected integer on lhs but got {}",
                                left
                            ));
                            (ERROR_TR_EXP, Type::Error)
                        }
                        (_, _) => {
                            ctx.flag_error_with_msg(format!(
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
                        ctx.flag_error_with_msg(format!(
                            "Expected integer on rhs but got {}",
                            right
                        ));
                        (ERROR_TR_EXP, Type::Error)
                    }
                    (_, Type::Int) => {
                        ctx.flag_error_with_msg(format!(
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
                        ctx.flag_error_with_msg(format!(
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
                        ctx.flag_error_with_msg(format!("{} only valid on Int, Record, Array or String types, but is used for {} and {}", oper, left, right));
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
        Exp::IntExp(i) => (translate::int_exp(i), Type::Int),
        Exp::VarExp(v) => trans_var(ctx, v),
        Exp::StringExp(s) => (translate::string_exp(ctx, s), Type::String),
        Exp::CallExp { func, args, pos } => {
            let fentry_opt = ctx.varfun_env.look(ctx.intern(func));
            if fentry_opt.is_none() {
                ctx.flag_error_with_msg(format!(
                    "Trying to call undeclared function {}",
                    ctx.get_span(func)
                ));
                return (ERROR_TR_EXP, Type::Error);
            } else if !matches!(fentry_opt.unwrap(), EnvEntry::FunEntry { .. }) {
                ctx.flag_error_with_msg(format!("{} is not a function!", ctx.get_span(func)));
                return (ERROR_TR_EXP, Type::Error);
            } else {
                let Some(EnvEntry::FunEntry { formals, result }) = fentry_opt;
                if formals.len() != args.len() {
                    ctx.flag_error_with_msg(format!(
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
                            || matches!(Type::Error, formals.get(i).unwrap())
                        {
                            ctx.flag_error();
                            break;
                        } else if !arg_ty.compatible_with(formals.get(i).unwrap()) {
                            ctx.flag_error_with_msg(format!(
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
                        (translate::call_exp("TODO"), result)
                    }
                }
            }
        }
        Exp::RecordExp { .. } => {
            todo!()
        }
        Exp::SeqExp(..) => {
            todo!()
        }
        Exp::AssignExp { var, exp, pos } => {
            todo!()
        }
        Exp::IfExp {
            test,
            then,
            els,
            pos,
        } => {
            todo!()
        }
        Exp::WhileExp { test, body, pos } => {
            todo!()
        }
        Exp::ForExp {
            var,
            escape,
            lo,
            hi,
            body,
            pos,
        } => {
            todo!()
        }
        Exp::BreakExp(..) => {
            todo!()
        }
        Exp::LetExp { decs, body, pos } => {
            todo!()
        }
        Exp::ArrayExp {
            typ,
            size,
            init,
            pos,
        } => {
            todo!()
        }
    }
}

fn trans_var(ctx: &mut TypeCheckingContext, var: &Var) -> TrExp {
    todo!()
}

fn trans_dec(ctx: &mut TypeCheckingContext, dec: &Dec) -> Vec<TrExp> {
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
    };

    let (exp, ty) = trans_exp(&mut ctx, n, None);
    match ty {
        Type::Error => Err(()),
        _ => Ok(exp),
    }
}
