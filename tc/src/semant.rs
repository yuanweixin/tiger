use cfgrammar::Span;
use itertools::Itertools;
use std::collections::HashMap;
use std::{cell::RefCell, num::NonZeroUsize, rc::Rc};

use crate::{
    absyn::types::{Dec, Exp, Oper, Ty, Var},
    escape, frame,
    frame::Frame,
    symbol::Symbol,
    symtab::SymbolTable,
    temp::{Label, Uuids, UuidsImpl},
    translate,
    translate::{Level, TrExp},
};
use strum_macros::Display;

// ((start line, start column), (end line, end column))
type LineCol = ((usize, usize), (usize, usize));
trait LineColInfo {
    fn start_line(&self) -> usize;
    fn end_line(&self) -> usize;
    fn start_column(&self) -> usize;
    fn end_column(&self) -> usize;
}

impl LineColInfo for LineCol {
    fn start_line(&self) -> usize {
        self.0 .0
    }
    fn end_line(&self) -> usize {
        self.1 .0
    }
    fn start_column(&self) -> usize {
        self.0 .1
    }
    fn end_column(&self) -> usize {
        self.1 .1
    }
}

#[inline]
fn error_type_check_output() -> (TrExp, Type) {
    // the first element is just some dummy.
    (TrExp::Cx(translate::Conditional::Falsy), Type::Error)
}

pub struct TypeCheckingContext<'a> {
    next_array_record_ord: NonZeroUsize,
    type_env: SymbolTable<Type>,
    varfun_env: SymbolTable<EnvEntry>,
    has_err: bool,
    input: &'a str,
    gen: &'a mut dyn Uuids,
    frags: Vec<frame::Frag>,
}

macro_rules! top_level_fn {
    ($name:expr, $frame:ident, $res:expr, $formals:expr, $gen:expr, $result_type:expr) => {
        let escapes = $formals.iter().map(|_| false).collect();
        let (level, label) = Level::new_level::<$frame>(Level::outermost(), escapes, $gen, $name);
        $res.enter(
            $gen.intern($name),
            EnvEntry::FunEntry {
                level,
                formals: Rc::new($formals),
                result: $result_type,
                label,
            },
        );
    };
}

impl<'a> TypeCheckingContext<'a> {
    fn new<T: Frame + 'static>(input: &'a str, gen: &'a mut dyn Uuids) -> Self {
        let type_env = Self::base_env_type_env(gen);
        let varfun_env = TypeCheckingContext::base_varfun_env::<T>(gen);
        Self {
            next_array_record_ord: NonZeroUsize::MIN,
            type_env,
            varfun_env,
            has_err: false,
            input,
            gen,
            frags: Vec::new(),
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

    fn base_env_type_env(gen: &mut dyn Uuids) -> SymbolTable<Type> {
        let mut res = SymbolTable::empty();
        res.begin_scope();
        res.enter(gen.intern("int"), Type::Int);
        res.enter(gen.intern("string"), Type::String);
        res
    }

    fn base_varfun_env<T: Frame + 'static>(gen: &mut dyn Uuids) -> SymbolTable<EnvEntry> {
        let mut res = SymbolTable::empty();
        res.begin_scope();
        top_level_fn!("print", T, res, vec![Type::String], gen, Type::Unit);
        top_level_fn!("flush", T, res, Vec::<Type>::new(), gen, Type::Unit);
        top_level_fn!("getChar", T, res, Vec::<Type>::new(), gen, Type::String);
        top_level_fn!("ord", T, res, vec![Type::String], gen, Type::Int);
        top_level_fn!("chr", T, res, vec![Type::Int], gen, Type::String);
        top_level_fn!("size", T, res, vec![Type::String], gen, Type::Int);
        top_level_fn!(
            "substring",
            T,
            res,
            vec![Type::String, Type::Int, Type::Int],
            gen,
            Type::String
        );
        top_level_fn!(
            "concat",
            T,
            res,
            vec![Type::String, Type::String],
            gen,
            Type::String
        );
        top_level_fn!("not", T, res, vec![Type::Int], gen, Type::Int);
        top_level_fn!("exit", T, res, vec![Type::Int], gen, Type::Unit);
        res
    }

    fn flag_error_with_msg(&mut self, pos: &LineCol, msg: &str) {
        self.has_err = true;
        println!(
            "line:{},col:{} error: {}",
            pos.start_line(),
            pos.start_column(),
            msg
        );
    }

    fn flag_error(&mut self) {
        self.has_err = true;
    }

    fn get_span(&self, s: &Span) -> &str {
        &self.input[s.start()..s.end()]
    }

    pub fn intern(&mut self, s: &Span) -> Symbol {
        // cannot call get_span here because it will borrow the self parameter as a immutable ref.
        // otoh, we can borrow the self.input here since it's a "separate" chunk of memory.
        let x = &self.input[s.start()..s.end()];
        self.gen.intern(x)
    }

    fn has_error(&self) -> bool {
        self.has_err
    }

    pub fn resolve_unchecked(&self, s: &Symbol) -> &str {
        self.gen.resolve(s).unwrap()
    }
}

#[derive(Eq, PartialEq, Display, Debug, Clone)]
pub enum Type {
    // a vec of (field name, field type name)
    // using a Weak<Type> was an option here but it had the difficulty of needing Eq defined.
    // also, that would force Type to be placed in Rc in containers.
    Record(Rc<Vec<(Symbol, Symbol)>>, NonZeroUsize),
    Nil,
    Int,
    String,
    // the type of the array element is symbol to support mutually recursive types.
    Array(Symbol, NonZeroUsize),
    Unit,
    Name(Symbol), // this is the "unresolved" reference to other types
    Error,
}

impl Type {
    fn compatible_with(&self, other: &Self) -> bool {
        match (self, other) {
            // error should not be compatible with anything even itself.
            // although, the typical usage pattern is to first check if we got any Type::Error
            // on hand, if so, we do not print more (noisy) error messages. the compatible with check
            // is really intended to detect that between non-Error types, so that we can trigger
            // type checking messages.
            (Type::Error, _) | (_, Type::Error) => false,
            (Type::Nil, Type::Record(_, _)) | (Type::Record(_, _), Type::Nil) => true,
            (a, b) => a == b,
        }
    }
}

// The translated IR and the result of the type check.
type ExpTy = (TrExp, Type);

#[derive(Clone, Debug, Display)]
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
                            (translate::binop(oper, lhs_ir, rhs_ir, ctx.gen), Type::Int)
                        }
                        (Type::Error, _) | (_, Type::Error) => error_type_check_output(),
                        (Type::Int, _) => {
                            ctx.flag_error_with_msg(
                                pos,
                                &format!("Expected integer on rhs but got {}", right),
                            );
                            error_type_check_output()
                        }
                        (_, Type::Int) => {
                            ctx.flag_error_with_msg(
                                pos,
                                &format!("Expected integer on lhs but got {}", left),
                            );
                            error_type_check_output()
                        }
                        (_, _) => {
                            ctx.flag_error_with_msg(
                                pos,
                                &format!(
                                    "Expected integer operands but got lhs={}, rhs={}",
                                    left, right
                                ),
                            );
                            error_type_check_output()
                        }
                    }
                }
                Oper::LtOp | Oper::LeOp | Oper::GtOp | Oper::GeOp => match (lhs_ty, rhs_ty) {
                    (Type::Int, Type::Int) => {
                        (translate::binop(oper, lhs_ir, rhs_ir, ctx.gen), Type::Int)
                    }
                    (Type::Error, _) | (_, Type::Error) => error_type_check_output(),
                    (Type::Int, _) => {
                        ctx.flag_error_with_msg(
                            pos,
                            &format!("Expected integer on rhs but got {}", right),
                        );
                        error_type_check_output()
                    }
                    (_, Type::Int) => {
                        ctx.flag_error_with_msg(
                            pos,
                            &format!("Expected integer on lhs but got {}", right),
                        );
                        error_type_check_output()
                    }
                    (_, _) => error_type_check_output(),
                },
                Oper::EqOp | Oper::NeqOp => {
                    if Type::Error == lhs_ty || Type::Error == rhs_ty {
                        error_type_check_output()
                    } else if !lhs_ty.compatible_with(&rhs_ty) {
                        ctx.flag_error_with_msg(
                            pos,
                            &format!(
                                "Types not compatible for comparison, lhs={}, rhs={}",
                                left, right
                            ),
                        );
                        error_type_check_output()
                    } else if !matches!(
                        lhs_ty,
                        Type::Int | Type::Record(..) | Type::Array(..) | Type::String
                    ) && !matches!(
                        rhs_ty,
                        Type::Int | Type::Record(..) | Type::Array(..) | Type::String
                    ) {
                        ctx.flag_error_with_msg (pos, &format!("{} only valid on Int, Record, Array or String types, but is used for {} and {}", oper, lhs_ty, rhs_ty));
                        error_type_check_output()
                    } else {
                        if let (Type::String, Type::String) = (lhs_ty, rhs_ty) {
                            (
                                translate::string_cmp::<T>(
                                    matches!(oper, Oper::EqOp),
                                    lhs_ir,
                                    rhs_ir,
                                    ctx.gen,
                                ),
                                Type::Int,
                            )
                        } else {
                            (translate::binop(oper, lhs_ir, rhs_ir, ctx.gen), Type::Int)
                        }
                    }
                }
            }
        }
        Exp::NilExp => (translate::nil_exp(), Type::Nil),
        Exp::IntExp(i) => (translate::int_exp(*i), Type::Int),
        Exp::VarExp(v) => trans_var::<T>(ctx, level.clone(), v, break_label),
        Exp::StringExp(s, pos) => {
            // sanity check; in tiger.l String is a regex that start and ends with '"'
            let mut x = &ctx.input[s.start()..s.end()];
            // the string from the lexer should contain the start and end quotes.
            debug_assert!(x.starts_with("\""));
            debug_assert!(x.ends_with("\""));
            x = x.trim_start_matches("\"");
            x = x.trim_end_matches("\"");
            if !x.is_ascii() {
                ctx.flag_error_with_msg(pos, "tiger only supports ascii strings");
                return error_type_check_output();
            }
            (
                translate::string_exp::<T>(&x, ctx.gen, &mut ctx.frags),
                Type::String,
            )
        }
        Exp::CallExp { func, args, pos } => {
            let sym = ctx.intern(func);
            let fentry_opt = ctx.varfun_env.look(sym).map(|e| e.clone());

            match fentry_opt {
                None => {
                    ctx.flag_error_with_msg(
                        pos,
                        &format!("Trying to call undeclared function {}", ctx.get_span(func)),
                    );
                    error_type_check_output()
                }
                Some(EnvEntry::FunEntry {
                    label,
                    level: callee_level,
                    formals,
                    result,
                    ..
                }) => {
                    if formals.len() != args.len() {
                        ctx.flag_error_with_msg(
                            pos,
                            &format!(
                                "Expected {} args for function {} but got {}",
                                formals.len(),
                                ctx.get_span(func),
                                args.len()
                            ),
                        );
                        error_type_check_output()
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
                                ctx.flag_error_with_msg(
                                    pos,
                                    &format!(
                                "Call to {} expects argument of type {} at position {} but got {}",
                                ctx.get_span(func),
                                formals.get(i).unwrap(),
                                i + 1,
                                arg_ty
                            ),
                                );
                            }
                            arg_irs.push(arg_ir);
                        }
                        if ctx.has_error() {
                            error_type_check_output()
                        } else {
                            (
                                translate::call_exp::<T>(
                                    label,
                                    level,
                                    arg_irs,
                                    callee_level,
                                    ctx.gen,
                                    result == Type::Unit,
                                ),
                                result.clone(),
                            )
                        }
                    }
                }
                Some(_) => {
                    ctx.flag_error_with_msg(
                        pos,
                        &format!("{} is not a function!", ctx.get_span(func)),
                    );
                    error_type_check_output()
                }
            }
        }
        Exp::RecordExp { fields, typ, pos } => {
            let sym = ctx.intern(typ);
            let rec_entry_opt = ctx.type_env.look(sym).map(|e| e.clone());

            match rec_entry_opt {
                None => {
                    ctx.flag_error_with_msg(
                        pos,
                        &format!("{} has not been declared.", ctx.get_span(typ)),
                    );
                    error_type_check_output()
                }
                Some(Type::Record(name_types, ord)) => {
                    if name_types.len() != fields.len() {
                        ctx.flag_error_with_msg (pos, &format!(
                        "Record {} is declared with {} fields, but {} are given at instantiation site",
                        ctx.get_span(typ),
                        name_types.len(),
                        fields.len()
                    ));
                        error_type_check_output()
                    } else {
                        let mut site_irs = Vec::new();

                        for (decl_sym, decl_typ_sym) in name_types.as_ref() {
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
                                    let decl_typ =
                                        ctx.type_env.look(*decl_typ_sym).map(|e| e.clone());
                                    if decl_typ.is_none() {
                                        ctx.flag_error_with_msg(
                                            pos,
                                            &format!(
                                                "field {} has undeclared type {}",
                                                ctx.resolve_unchecked(decl_sym),
                                                ctx.resolve_unchecked(&decl_typ_sym)
                                            ),
                                        );
                                    } else if !site_ty.compatible_with(&decl_typ.as_ref().unwrap())
                                    {
                                        ctx.flag_error();
                                        if !matches!(site_ty, Type::Error) {
                                            ctx.flag_error_with_msg (pos, &format!("field {} is declared with type {} but is used with type {}", ctx.get_span(site_sym_span), decl_typ.unwrap(), site_ty));
                                        }
                                    } else {
                                        // only push when no typing error occur.
                                        // this means we can check length vs the declared number of record fields
                                        // to see if type check error happened.
                                        site_irs.push(site_ir);
                                    }
                                    break;
                                }
                            }
                            if !found {
                                ctx.flag_error_with_msg (pos, &format!(
                                    "record field {} is used but not part of the record declaration",
                                    ctx.resolve_unchecked(decl_sym)
                                ));
                            }
                        }

                        if site_irs.len() != fields.len() {
                            error_type_check_output()
                        } else {
                            (
                                translate::record_exp::<T>(site_irs, ctx.gen),
                                Type::Record(name_types.clone(), ord.clone()),
                            )
                        }
                    }
                }
                _ => {
                    ctx.flag_error_with_msg(
                        pos,
                        &format!("{} is not a record type.", ctx.get_span(typ)),
                    );
                    error_type_check_output()
                }
            }
        }
        Exp::SeqExp(exps) => {
            let mut ret_val = Type::Unit;
            let mut exp_irs = Vec::new();

            // TODO patch up with type messages on the individual exps.
            let mut has_err = false;
            for exp in exps {
                let (exp_ir, exp_ty) = trans_exp::<T>(ctx, level.clone(), exp, break_label);
                has_err |= exp_ty == Type::Error;
                ret_val = exp_ty;
                exp_irs.push(exp_ir);
            }
            if has_err {
                error_type_check_output()
            } else {
                (
                    translate::seq_exp(exp_irs, !matches!(ret_val, Type::Unit), ctx.gen),
                    ret_val,
                )
            }
        }
        Exp::AssignExp { var, exp, pos } => {
            // nil can be assigned to record types.
            let (dst_ir, dst_ty) = trans_var::<T>(ctx, level.clone(), var, break_label);
            let (src_ir, src_ty) = trans_exp::<T>(ctx, level.clone(), exp, break_label);

            if matches!(dst_ty, Type::Error) || matches!(src_ty, Type::Error) {
                error_type_check_output()
            } else if matches!(src_ty, Type::Nil) && !matches!(dst_ty, Type::Record(..)) {
                ctx.flag_error_with_msg(
                    pos,
                    &format!(
                        "Nil can only be assigned to record type, but is assigned to {} here.",
                        dst_ty
                    ),
                );
                error_type_check_output()
            } else if !dst_ty.compatible_with(&src_ty) {
                ctx.flag_error_with_msg(
                    pos,
                    &format!(
                        "Assigning incompatible types: dst={}, src={}",
                        dst_ty, src_ty
                    ),
                );
                error_type_check_output()
            } else {
                match var.as_ref() {
                    Var::SimpleVar(span, pos) => {
                        let sym = ctx.intern(span);
                        let var_entry = ctx.varfun_env.look(sym).map(|e| e.clone());

                        match var_entry {
                            None => {
                                ctx.flag_error_with_msg(
                                    pos,
                                    &format!(
                                    "the variable {} needs to be declared before being assigned to",
                                    ctx.get_span(span)
                                ),
                                );
                                error_type_check_output()
                            }
                            Some(EnvEntry::FunEntry { .. }) => {
                                ctx.flag_error_with_msg(pos, "Cannot assign to a function");
                                error_type_check_output()
                            }
                            Some(EnvEntry::VarEntry { ty, readonly, .. }) => {
                                if readonly {
                                    ctx.flag_error_with_msg(
                                        pos,
                                        "Cannot assign to readonly location",
                                    );
                                    error_type_check_output()
                                } else if matches!(ty, Type::Error) {
                                    error_type_check_output()
                                } else {
                                    (translate::assignment(dst_ir, src_ir, ctx.gen), Type::Unit)
                                }
                            }
                        }
                    }
                    _ => (translate::assignment(dst_ir, src_ir, ctx.gen), Type::Unit),
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
            if matches!(test_ty, Type::Error) {
                error_type_check_output()
            } else if !matches!(test_ty, Type::Int) {
                ctx.flag_error_with_msg(
                    pos,
                    &format!("Conditionals must evaluate to INT but got {} here", test_ty),
                );
                error_type_check_output()
            } else {
                let (then_ir, then_ty) =
                    trans_exp::<T>(ctx, level.clone(), then.as_ref(), break_label);
                if els.is_none() {
                    if !matches!(then_ty, Type::Unit) {
                        ctx.flag_error_with_msg(
                            pos,
                            &format!(
                                "if-then must evaluate to Unit, but has type {} here",
                                then_ty
                            ),
                        );
                        error_type_check_output()
                    } else {
                        (
                            translate::conditional(cond_ir, then_ir, None, ctx.gen),
                            then_ty,
                        )
                    }
                } else {
                    let (else_ir, else_ty) =
                        trans_exp::<T>(ctx, level.clone(), els.as_ref().unwrap(), break_label);
                    if !else_ty.compatible_with(&then_ty) {
                        ctx.flag_error_with_msg (pos, &format!("if-then-else branches have incompatible types: then has type {}, else has type {}", then_ty, else_ty));
                        error_type_check_output()
                    } else {
                        (
                            // TODO handle the Nil branch type in `conditional`
                            // in order to generate more efficient IR.
                            translate::conditional(cond_ir, then_ir, Some(else_ir), ctx.gen),
                            then_ty,
                        )
                    }
                }
            }
        }
        Exp::WhileExp { test, body, pos } => {
            let while_done_label = Some(ctx.gen.new_unnamed_label());
            // break is legal in expressions. so if they try to break in the condition
            // of a while loop, it is interpreted as breaking to the end of this while loop,
            // since logically, the while condition is re-evaluated each iteration.
            let (cond_ir, cond_ty) = trans_exp::<T>(ctx, level.clone(), test, while_done_label);
            if matches!(cond_ty, Type::Error) {
                error_type_check_output()
            } else if !matches!(cond_ty, Type::Int) {
                ctx.flag_error_with_msg(
                    pos,
                    &format!("while condition must be of Int but got {}", cond_ty),
                );
                error_type_check_output()
            } else {
                let (body_ir, body_ty) = trans_exp::<T>(ctx, level.clone(), body, while_done_label);
                match body_ty {
                    Type::Unit => (
                        translate::while_loop(cond_ir, body_ir, while_done_label.unwrap(), ctx.gen),
                        Type::Unit,
                    ),
                    Type::Error => error_type_check_output(),
                    _ => {
                        ctx.flag_error_with_msg(pos, "while body must not produce a value");
                        error_type_check_output()
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
            let for_done_label = Some(ctx.gen.new_unnamed_label());
            // if "break" happens in evaluating the for loop params, will just break the loop itself.
            let (lo_ir, lo_ty) = trans_exp::<T>(ctx, level.clone(), lo, for_done_label);
            let (hi_ir, hi_ty) = trans_exp::<T>(ctx, level.clone(), hi, for_done_label);
            let mut err = false;
            if !matches!(lo_ty, Type::Int) {
                err = true;
                ctx.flag_error_with_msg(pos, "lo in for loop range must be of integral type");
            }
            if !matches!(hi_ty, Type::Int) {
                err = true;
                ctx.flag_error_with_msg(pos, "lo in for loop range must be of integral type");
            }

            ctx.varfun_env.begin_scope();

            let sym = ctx.intern(var);
            let acc = Level::alloc_local(level.clone(), *escape, ctx.gen);

            // TODO revisit escape for the loop counter. can it escape? obviously can read it in nested.
            //
            ctx.varfun_env.enter(
                sym,
                EnvEntry::VarEntry {
                    access: acc.clone(),
                    ty: Type::Int,
                    readonly: true,
                },
            );
            let (body_ir, body_ty) = trans_exp::<T>(ctx, level.clone(), body, for_done_label);
            ctx.varfun_env.end_scope();

            match body_ty {
                Type::Unit => {
                    if err {
                        error_type_check_output()
                    } else {
                        (
                            translate::for_loop::<T>(
                                acc.1,
                                lo_ir,
                                hi_ir,
                                body_ir,
                                for_done_label.unwrap(),
                                ctx.gen,
                            ),
                            Type::Unit,
                        )
                    }
                }
                Type::Error => error_type_check_output(),
                _ => {
                    ctx.flag_error_with_msg(pos, "for body must not produce a value");
                    error_type_check_output()
                }
            }
        }
        Exp::BreakExp(pos) => {
            if break_label.is_none() {
                ctx.flag_error_with_msg(pos, "naked break statement");
                error_type_check_output()
            } else {
                (translate::break_stmt(break_label.unwrap()), Type::Unit)
            }
        }
        Exp::LetExp { decs, body, .. } => {
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

            (
                translate::let_exp(var_init_irs, let_body_ir, ctx.gen),
                let_body_ty,
            )
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
                ctx.flag_error_with_msg(
                    pos,
                    &format!(
                        "Trying to use an undeclared array type {}",
                        ctx.get_span(typ)
                    ),
                );
                error_type_check_output()
            } else {
                match arr_ty_opt.as_ref().unwrap() {
                    Type::Array(ele_ty_sym, _) => {
                        let (size_ir, size_ty) =
                            trans_exp::<T>(ctx, level.clone(), size, break_label);
                        match size_ty {
                            Type::Int => {
                                let (init_val_ir, init_val_ty) =
                                    trans_exp::<T>(ctx, level.clone(), init, break_label);

                                let ele_ty = ctx.type_env.look(*ele_ty_sym);
                                if ele_ty.is_none() {
                                    ctx.flag_error_with_msg(
                                        pos,
                                        &format!(
                                            "array element has undeclared type {}",
                                            ctx.resolve_unchecked(&ele_ty_sym)
                                        ),
                                    );
                                    error_type_check_output()
                                } else if !init_val_ty.compatible_with(ele_ty.unwrap()) {
                                    ctx.flag_error_with_msg (pos, &format!(
                                        "array initialized with type {} but declared with type {}, init={:?}",
                                        init_val_ty, ele_ty.unwrap(), init
                                    ));
                                    error_type_check_output()
                                } else {
                                    (
                                        translate::array_exp::<T>(size_ir, init_val_ir, ctx.gen),
                                        arr_ty_opt.unwrap().clone(),
                                    )
                                }
                            }
                            _ => {
                                ctx.flag_error_with_msg(pos, "array length is not an int");
                                error_type_check_output()
                            }
                        }
                    }
                    _ => {
                        ctx.flag_error_with_msg(
                            pos,
                            &format!("{} is not of array type!", ctx.get_span(typ)),
                        );
                        error_type_check_output()
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
                    ctx.flag_error_with_msg(
                        pos,
                        &format!("use of undeclared variable {}", ctx.get_span(span)),
                    );
                    error_type_check_output()
                }
                Some(EnvEntry::FunEntry { .. }) => {
                    // this version of tiger does not support using functions as lvalues.
                    // functions can only be called.
                    // therefore, this is an error.
                    ctx.flag_error_with_msg(
                        pos,
                        &format!(
                            "attempting to use function {} as a lvalue",
                            ctx.get_span(span)
                        ),
                    );
                    error_type_check_output()
                }
                Some(EnvEntry::VarEntry { access, ty, .. }) => match ty {
                    Type::Error => error_type_check_output(),
                    _ => (
                        translate::simple_var::<T>(access.clone(), level, ctx.gen),
                        ty.clone(),
                    ),
                },
            }
        }
        Var::FieldVar(lhs_var, rhs_span, pos) => {
            let (lhs_var_ir, lhs_var_ty) = trans_var::<T>(ctx, level.clone(), lhs_var, break_label);
            match lhs_var_ty {
                Type::Record(field_types, _) => {
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
                            ctx.flag_error_with_msg(
                                pos,
                                &format!("{} is not a field of the record", ctx.get_span(rhs_span)),
                            );
                            error_type_check_output()
                        }
                        Some(s) => {
                            // in our cute toy language every record field
                            //  is scalar or pointer so they have same
                            //  size, so we don't even have to do any
                            // extra work calculating the record size.
                            (
                                translate::record_field::<T>(lhs_var_ir, field_offset, ctx.gen),
                                ctx.type_env.look(*s).unwrap().clone(),
                            )
                        }
                    }
                }
                x => {
                    ctx.flag_error_with_msg(
                        pos,
                        &format!(
                            "tried to access field {} of a non-record type {}",
                            ctx.get_span(rhs_span),
                            x
                        ),
                    );
                    error_type_check_output()
                }
            }
        }
        Var::SubscriptVar(deref_var, index_exp, pos) => {
            let (lhs_ir, lhs_ty) = trans_var::<T>(ctx, level.clone(), deref_var, break_label);
            match lhs_ty {
                Type::Array(ele_ty, _) => {
                    let (idx_ir, idx_ty) =
                        trans_exp::<T>(ctx, level.clone(), index_exp, break_label);

                    match idx_ty {
                        Type::Int => {
                            let subscript_ir =
                                translate::subscript_var::<T>(lhs_ir, idx_ir, ctx.gen);
                            (subscript_ir, ctx.type_env.look(ele_ty).unwrap().clone())
                        }
                        Type::Error => error_type_check_output(),
                        _ => {
                            ctx.flag_error_with_msg(pos, "array index must be of Int type!");
                            error_type_check_output()
                        }
                    }
                }
                Type::Error => error_type_check_output(),
                x => {
                    ctx.flag_error_with_msg(
                        pos,
                        &format!("Subscript is only valid for array, used on {}", x),
                    );
                    error_type_check_output()
                }
            }
        }
    }
}

fn ty_to_type(ctx: &mut TypeCheckingContext, ty: &Ty, _pos: &LineCol) -> Type {
    // return the translated type, as well as whether type is forward referencing some unknown type.
    match ty {
        Ty::NameTy(span, _) => {
            let sym = ctx.intern(&span);
            match ctx.type_env.look(sym) {
                None => Type::Name(sym),
                // collapse the name one level.
                Some(Type::Name(y)) => Type::Name(*y),
                Some(x) => x.clone(),
            }
        }
        Ty::ArrayTy(span, _) => {
            let sym = ctx.intern(&span);
            match ctx.type_env.look(sym) {
                None => Type::Array(sym, ctx.get_next_array_record_ord()),
                // collapse the name one level.
                Some(Type::Name(y)) => Type::Array(*y, ctx.get_next_array_record_ord()),
                Some(_) => Type::Array(sym, ctx.get_next_array_record_ord()),
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
                    ctx.flag_error_with_msg(
                        &field.pos,
                        &format!("field {} declared more than once in record.", field_name),
                    );
                } else {
                    seen.insert(field_name, ());
                    let field_sym = ctx.intern(&field.name);
                    let typ_sym = ctx.intern(&field.typ);
                    let type_opt = ctx.type_env.look(typ_sym);
                    match type_opt {
                        None => record_field_types.push((field_sym, typ_sym)),
                        // collapse the name one level.
                        Some(Type::Name(y)) => record_field_types.push((field_sym, *y)),
                        Some(_) => record_field_types.push((field_sym, typ_sym)),
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
                let fun_name = &ctx.input[fundec.name.start()..fundec.name.end()];
                if seen.contains_key(fun_name) {
                    ctx.flag_error_with_msg (&fundec.pos, &format!("{} is declared more than once in a sequence of mutually recursive functions", fun_name));
                } else {
                    seen.insert(fun_name, ());
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
                    let typ_name = ctx.intern(&field.typ);
                    let field_type_opt = ctx.type_env.look(typ_name);
                    match field_type_opt {
                        None => {
                            ctx.flag_error_with_msg(
                                &field.pos,
                                &format!(
                                    "function parameter {} has unknown type {}",
                                    ctx.get_span(&field.name),
                                    ctx.get_span(&field.typ)
                                ),
                            );
                            formal_types.push(Type::Error);
                        }
                        Some(ty) => {
                            formal_types.push(ty.clone());
                        }
                    }
                    escapes.push(field.escape);
                }
                let (new_level, new_level_label) =
                    Level::new_level::<T>(level.clone(), escapes, ctx.gen, fun_name);
                let funentry = EnvEntry::FunEntry {
                    level: new_level,
                    label: new_level_label,
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
                        formals,
                        result,
                        ..
                    }) => {
                        for i in 0..fundec.params.len() {
                            let var_entry = EnvEntry::VarEntry {
                                ty: formals[i].clone(),
                                readonly: true,
                                access: translate::Access(
                                    level.clone(),
                                    level.borrow().formal_without_static_link(i),
                                ),
                            };
                            let sym = ctx.intern(&fundec.params[i].name);
                            ctx.varfun_env.enter(sym, var_entry);
                        }

                        // appendix: a break in procedure p cannot terminate a loop in procedure q, even
                        // if p is nsted within q.
                        //
                        // in other words, breaks do not work across function boundaries.
                        let (fun_body_ir, fun_body_ty) =
                            trans_exp::<T>(ctx, level.clone(), &*fundec.body, None);
                        if matches!(fun_body_ty, Type::Error) || matches!(result, Type::Error) {
                            // error msg already printed when the error first appeared, and ctx already
                            // flagged, so just move on and check the other fundecs.
                            continue;
                        } else if !fun_body_ty.compatible_with(&result) {
                            ctx.flag_error_with_msg(
                                &fundec.pos,
                                &format!(
                                    "expected return type {} for function but got {}",
                                    result, fun_body_ty
                                ),
                            );
                        } else {
                            // if function has return value, we will have to indicate it so that the
                            // ir generator will tack on a Move to the return value register at the end
                            // of the function.
                            let has_return_val = match result {
                                Type::Array(..) | Type::Int | Type::String | Type::Record(..) => {
                                    true
                                }
                                Type::Error | Type::Unit | Type::Name(..) | Type::Nil => false,
                            };

                            // this has the side effect of adding this function to fragment list.
                            translate::proc_entry_exit::<T>(
                                has_return_val,
                                level.clone(),
                                fun_body_ir,
                                &mut ctx.frags,
                                ctx.gen,
                            );
                        }
                    }
                }
                ctx.varfun_env.end_scope();
            }
            None
        }
        Dec::VarDec {
            name,
            escape,
            typ: decl_typ,
            init,
            pos,
        } => {
            let var_name_sym = ctx.intern(name);
            let name = &ctx.input[name.start()..name.end()];
            let (init_ir, init_ty) = trans_exp::<T>(ctx, level.clone(), init, break_label);

            match decl_typ {
                None => {
                    if matches!(init_ty, Type::Nil) {
                        ctx.flag_error_with_msg (pos, &format!("Variable {} is declared with unknown type and initiated with nil. Fix by using the long form, e.g. var {} : <your-type> = ...", name, name));
                    } else {
                        // no return type specified, infer it
                        let acc = Level::alloc_local(level, *escape, ctx.gen);
                        ctx.varfun_env.enter(
                            var_name_sym,
                            EnvEntry::VarEntry {
                                ty: init_ty,
                                readonly: false,
                                access: acc.clone(),
                            },
                        );
                        return Some(translate::var_dec_assignment::<T>(acc.1, init_ir, ctx.gen));
                    }
                }
                Some((span, pos)) => {
                    let var_typ_sym = ctx.intern(span);
                    match ctx.type_env.look(var_typ_sym) {
                        None => {
                            ctx.flag_error_with_msg(
                                pos,
                                &format!("unknown return type {}", ctx.get_span(span)),
                            );
                            ctx.varfun_env.enter(
                                var_name_sym,
                                EnvEntry::VarEntry {
                                    ty: Type::Error,
                                    readonly: true,
                                    // access is a dummy value because type checking failed
                                    access: translate::Access(
                                        level.clone(),
                                        frame::Access::InFrame(42),
                                    ),
                                },
                            );
                        }
                        Some(t) => {
                            if !t.compatible_with(&init_ty) {
                                ctx.flag_error_with_msg (pos, &format!("return type of {} does not match actual type {} of initializer.", init_ty, t));
                                ctx.varfun_env.enter(
                                    var_name_sym,
                                    EnvEntry::VarEntry {
                                        ty: Type::Error,
                                        readonly: true,
                                        // access is a dummy value because type checking failed
                                        access: translate::Access(
                                            level.clone(),
                                            frame::Access::InFrame(42),
                                        ),
                                    },
                                );
                            } else {
                                let acc = Level::alloc_local(level, *escape, ctx.gen);
                                ctx.varfun_env.enter(
                                    var_name_sym,
                                    EnvEntry::VarEntry {
                                        ty: t.clone(),
                                        readonly: false,
                                        access: acc.clone(),
                                    },
                                );
                                return Some(translate::var_dec_assignment::<T>(
                                    acc.1, init_ir, ctx.gen,
                                ));
                            }
                        }
                    }
                }
            }
            // if it falls through, some error occurred.
            None
        }

        Dec::TypeDec(tydecs) => {
            // spec from appendix:
            // mutually recursive types are declared by a conseq seq of type dec
            // without intervening value or func dec. Each recursion cycle must
            // pass through a record or array type.
            // ...
            // no two types in a seq of mutually recursive types may have the
            // same name.

            let mut seen = Vec::new();
            let mut name_ty_syms = Vec::new();
            let mut arr_record_ty_unref_syms = Vec::new();

            for dec in tydecs {
                let name = &ctx.input[dec.name.start()..dec.name.end()];
                let type_decl_name = ctx.intern(&dec.name);
                if seen.contains(&type_decl_name) {
                    ctx.flag_error_with_msg(
                        &dec.pos,
                        &format!(
                        "{} is declared more than once in a sequence of mutually recursive types.",
                        name
                    ),
                    );
                    ctx.type_env.enter(type_decl_name, Type::Error);
                } else {
                    seen.push(type_decl_name);
                    let ty = ty_to_type(ctx, &*dec.ty, &dec.pos);
                    ctx.type_env.enter(type_decl_name, ty.clone());
                    match ty {
                        Type::Name(_) => name_ty_syms.push((type_decl_name, dec.pos)),
                        Type::Array(s, _) => {
                            // naive impl does look up all the time, but it is simple
                            match ctx.type_env.look(s) {
                                None => arr_record_ty_unref_syms.push((type_decl_name, s, dec.pos)),
                                Some(Type::Name(s)) => {
                                    arr_record_ty_unref_syms.push((type_decl_name, *s, dec.pos))
                                }
                                Some(_) => {}
                            }
                        }
                        Type::Record(v, _) => {
                            for (_, t) in v.as_ref() {
                                // naive impl does look up all the time, but it is simple
                                match ctx.type_env.look(*t) {
                                    None => {
                                        arr_record_ty_unref_syms.push((type_decl_name, *t, dec.pos))
                                    }
                                    Some(Type::Name(t)) => {
                                        arr_record_ty_unref_syms.push((type_decl_name, *t, dec.pos))
                                    }
                                    Some(_) => {}
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            // A cycle involving N types will involve all Type::Name entries.
            // Only Type::Name can participate in a cycle, as it is okay to have circular references
            // through a Type::Array or Type::Record. In the case where no such cycles exist, this loop
            // also has the side effect of resolving the Type::Name. Resolving means, once the final type ft
            // (the first non Type::Name) is determined, we can backtrack and, for the type name symbol tns
            // of each Type::Name encountered along the way, we enter the (tns, ft) into the type env.
            for (type_decl_name_sym, pos) in name_ty_syms {
                seen.clear();
                let name = ctx.resolve_unchecked(&type_decl_name_sym);
                let mut ty = ctx.type_env.look(type_decl_name_sym).unwrap().clone(); // safe because we updated type_env in above loop.
                seen.push(type_decl_name_sym);
                while let Type::Name(s) = ty {
                    if seen.contains(&s) {
                        ctx.flag_error_with_msg(
                            &pos,
                            &format!("circular type definition detected for type {}", name),
                        );
                        ty = Type::Error;
                        break;
                    }
                    seen.push(s);
                    match ctx.type_env.look(s) {
                        None => {
                            let x = ctx.resolve_unchecked(&s);
                            ctx.flag_error_with_msg(
                                &pos,
                                &format!(
                                    "type {} references type {}, which does not exist",
                                    name, x
                                ),
                            );
                            ty = Type::Error;
                            break;
                        }
                        Some(t) => {
                            ty = t.clone();
                        }
                    }
                }
                for sym in &seen {
                    ctx.type_env.enter(*sym, ty.clone());
                }
            }
            for (ty_decl_sym, group) in &arr_record_ty_unref_syms
                .into_iter()
                .group_by(|(ty_decl_sym, _, _)| *ty_decl_sym)
            {
                for (_, unresolved, pos) in group {
                    let ty = ctx.type_env.look(unresolved);
                    match ty {
                        None => {
                            ctx.flag_error_with_msg(
                                &pos,
                                &format!(
                                    "type {} is undeclared",
                                    ctx.resolve_unchecked(&unresolved)
                                ),
                            );
                            ctx.type_env.enter(ty_decl_sym, Type::Error);
                            break;
                        }
                        Some(Type::Error) => {
                            // propagate the error
                            ctx.type_env.enter(ty_decl_sym, Type::Error);
                            break;
                        }
                        Some(t) => {
                            ctx.type_env.enter(ty_decl_sym, t.clone());
                        }
                    }
                }
            }

            None
        }
    }
}

pub fn translate<T: Frame + 'static>(
    input: &str,
    ast: &mut Exp,
    gen: &mut dyn Uuids,
) -> Result<Vec<frame::Frag>, ()> {
    let mut ctx = TypeCheckingContext::new::<T>(input, gen);

    escape::find_escapes(&mut ctx, ast);

    let (main_level, ..) =
        Level::new_level::<T>(Level::outermost(), Vec::new(), ctx.gen, "tigermain");
    let (tigermain_ir, tigermain_ty) = trans_exp::<T>(&mut ctx, main_level.clone(), ast, None);

    // if main evaluate to an int, we can move that as the return value.
    // other types of return values are not sensibile as int, so just use 0.
    let tigermain_return_val_ir = if tigermain_ty == Type::Int {
        translate::add_int_return_value::<T>(tigermain_ir, ctx.gen)
    } else {
        translate::add_zero_return_value::<T>(tigermain_ir, ctx.gen)
    };
    match &*main_level.borrow() {
        Level::Top => unreachable!(),
        Level::Nested { .. } => {
            translate::proc_entry_exit::<T>(
                false, // there's no sensible value to interpret the main body as, so don't use it as a return value.
                main_level.clone(),
                tigermain_return_val_ir,
                &mut ctx.frags,
                ctx.gen,
            );
        }
    }

    if ctx.has_error() {
        Err(())
    } else {
        Ok(ctx.frags)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        frame,
        frame::{Escapes, Frame},
        ir::{IrExp, IrStm},
        temp::{self, Label, Uuids},
        util,
    };
    use std::path::PathBuf;

    use lrlex::lrlex_mod;
    use lrpar::lrpar_mod;
    use std::panic;
    use std::{fs, fs::DirEntry};

    lrlex_mod!("tiger.l");
    lrpar_mod!("tiger.y");

    #[derive(Debug)]
    struct TestFrame {
        name: Label,
        formals: Vec<frame::Access>,
        next_offset: i32,
    }

    const FP: &str = "fp";
    const RV: &str = "rv";

    impl Frame for TestFrame {
        fn return_value_register(gen: &mut dyn Uuids) -> temp::Temp
        where
            Self: Sized,
        {
            gen.named_temp(RV)
        }

        fn temp_map(gen: &mut dyn Uuids) -> temp::TempMap
        where
            Self: Sized,
        {
            unreachable!()
        }

        fn external_call(name: Label, exps: Vec<crate::ir::IrExp>) -> crate::ir::IrExp
        where
            Self: Sized,
        {
            IrExp::Call(Box::new(IrExp::Name(name)), exps)
        }

        fn word_size() -> usize
        where
            Self: Sized,
        {
            4
        }

        fn registers() -> &'static [frame::Register]
        where
            Self: Sized,
        {
            unreachable!()
        }

        fn string(_: temp::Label, _: &str) -> String
        where
            Self: Sized,
        {
            unreachable!()
        }

        fn frame_pointer(_: &mut dyn Uuids) -> temp::Temp
        where
            Self: Sized,
        {
            temp::test_helpers::new_unnamed_temp(1)
        }

        fn proc_entry_exit1(&mut self, x: IrStm, _: &mut dyn Uuids) -> IrStm {
            // just don't add stuff to the body for test purpose.
            x
        }

        fn proc_entry_exit2(&self, instrs: &mut Vec<crate::assem::Instr>, g: &mut dyn Uuids) {
            unreachable!()
        }

        fn proc_entry_exit3(
            &self,
            _: &Vec<crate::assem::Instr>,
            _: &mut dyn Uuids,
            _: temp::Label,
        ) -> (frame::Prologue, frame::Epilogue) {
            unreachable!()
        }

        fn new(_: Label, formals: Vec<Escapes>, gen: &mut dyn Uuids) -> Self {
            let frame_formals = formals.iter().map(|_| frame::Access::InFrame(42)).collect();
            TestFrame {
                name: gen.new_unnamed_label(),
                formals: frame_formals,
                next_offset: -4,
            }
        }
        fn name(&self) -> Label {
            self.name
        }

        fn formals(&self) -> &[frame::Access] {
            &self.formals
        }
        fn alloc_local(&mut self, _: Escapes, _: &mut dyn Uuids) -> frame::Access {
            let n = self.next_offset;
            self.next_offset -= TestFrame::word_size() as i32;
            frame::Access::InFrame(n)
        }
    }

    fn test_input_helper(input: &str, is_good: bool, dir_path: Option<&str>) -> Exp {
        let lexerderef = tiger_l::lexerdef();
        let lexer = lexerderef.lexer(input);
        let (res, errs) = tiger_y::parse(&lexer);
        assert!(errs.len() == 0, "{} path={:?}", input, dir_path);
        assert!(res.is_some(), "{}", input);
        assert!(res.as_ref().unwrap().is_ok(), "{}", input);

        let mut ast = res.unwrap().unwrap();

        let mut gen: UuidsImpl = Uuids::new();

        let result = panic::catch_unwind(move || {
            let res = super::translate::<TestFrame>(input, &mut ast, &mut gen);
            match (is_good, res) {
                (false, Ok(..)) => {
                    panic!(
                        "{} type checks but expected not to, ast={:?}, path={:?}",
                        input, ast, dir_path
                    );
                }
                (true, Err(..)) => {
                    panic!(
                        "{} fails to type check but was expected to, ast={:?}, path={:?}",
                        input, ast, dir_path
                    );
                }
                _ => {}
            }
            ast
        });
        if !result.is_ok() {
            println!(
                "`translate` panicked! error was {:#?}",
                result.as_ref().unwrap_err()
            );
            println!("`translate` panicked! input was {}", input);
            assert!(false);
        }
        // some dummy value.
        result.unwrap()
    }

    fn test_file(path: PathBuf, is_good: bool) {
        let input = fs::read_to_string(path.clone()).unwrap();
        test_input_helper(&input, is_good, path.to_str());
    }

    #[test]
    fn appel_good_programs() {
        let paths = util::get_tig_files_in("tests/tiger_programs/semant/good");

        for path in paths {
            println!("Testing path {:?}", &path);
            test_file(path, true);
        }
    }

    #[test]
    fn appel_bad_programs() {
        let paths = util::get_tig_files_in("tests/tiger_programs/semant/bad");

        for path in paths {
            test_file(path, false);
        }
    }

    #[test]
    fn more_good() {
        let inputs = [
            (
                "valid nil usage in appendix",
                r"let
    type any = {any: int}
    var a : any := nil
    function f(a: any) : int = 100
    in
    a := nil;
    if a <> nil then 1 else 1;
    if nil <> a then 2 else 2;
    if a = nil then 3 else 3;
    if nil = a then 4 else 4;
    f(nil)
    end",
            ),
            (
                "nil assignment in record",
                r"let
    type any = {any: int, x: any}
    var a : any := any{any=1, x=nil}
    in
    42
    end",
            ),
            (
                "if then both nil",
                r"let
    type any = {any: int}
    in
    if 1 = 1 then nil else nil
    end",
            ),
            (
                "if return non nil then return nil",
                r"let
    type any = {any: int}
    var a: any := any{any=1}
    in
    if 1 = 1 then a else nil
    end",
            ),
            (
                "if return nil then non nil",
                r"let
    type any = {any: int}
    var a: any := any{any=1}
    in
    if 1 = 1 then nil else a
    end",
            ),
            (
                "function a() : some_record = ... where body returns nil",
                r"let
    type any = {any: int}
    function f(a: any) : any = nil
    in
    f(nil)
    end",
            ),
            (
                "function a() = () should be accepted",
                r"let
    function f() = ()
    in
    f()
    end",
            ),
            (
                "local redeclarations, appendix example",
                r"let
        function print(v:int) = ()
        function f(v:int) =
        let var v := 6
            in print(v);
            let var v := 7 in print (v) end;
            print(v);
            let var v := 8 in print (v) end;
            print (v)
        end
    in
    ()
    end",
            ),
            (
                "break in a while loop is legal",
                r"
    let
    in
        while 1 = 1 do
            break
    end",
            ),
            (
                "break in a for loop is legal",
                r"let
    in
        for i :=0 to 100 do
            break
    end",
            ),
            (
                "standard library calls",
                r#"
    let
    in
        print("die");
        flush();
        getChar();
        ord("");
        chr(0);
        size("");
        substring("hello", 0, 1);
        concat("h","i");
        not(1);
        exit(1)
    end"#,
            ),
            (
                "circular self through array okay",
                r"let
        type a = array of a
    in
    end",
            ),
            (
                "circular self through record okay",
                r"
    let
        type a = {x: a}
    in
    end",
            ),
            (
                "circular nonself through array ok",
                r"
    let
        type a = b
        type b = array of a
    in
    end",
            ),
            (
                "circular nonself through record ok",
                r"
    let
        type a = b
        type b = {x: a}
    in
    end",
            ),
            (
                "redeclaration",
                r"
    let
        var i := 1
        var i := 2 /* i above irrelevant for escape analysis */
        var k := 3
        /* function i's scope effectively at start of recursive blk */
        function j () =
            (i();
            k + 1;
            ())
        function i () =
            (2;
            ())
        var i := 4
        function j () : int =
            i + 1  /* i escapes here */
    in
    end
    ",
            ),
            (
                "functions returning array okay",
                r"
    let
            type intArray = array of int
            function x() : intArray =
                intArray[7] of 9
    in
    end
    ",
            ),
            (
                "function returning record okay",
                r"
    let
            type rec = {i: int}
            function x() : rec =
                rec{i=42}
    in
    end
    ",
            ),
        ];
        for (_, input) in inputs {
            test_input_helper(input, true, None);
        }
    }

    #[test]
    fn more_bad() {
        let inputs = [
            (
                "",
                r"
            let
            var a := nil
            in 1
            end
        ",
            ),
            (
                "circular to self bad",
                r"
    let
        type a = a
    in
    end",
            ),
            (
                "circular through seq of type decls bad",
                r"
    let
        type a = b
        type b = a
    in
    end",
            ),
            (
                "fundec block shadow var name",
                r"
    let
        var i := 1
        function j () =
            i := 2 /* this is illegal because i is a func */
        function i () =  /* i defined as func at start of j */
            ()
    in
    end
    ",
            ),
            (
                "for loop bad input to access for loop var in lo or hi",
                r"
    let
        function x() =
            for i :=  let function j() = i:= 1 in 1 end to let function j() = i:= 100 in 100 end
            do
                ()
    in
    end
    ",
            ),
            (
                "for loop counter cannot be assigned to",
                r"let
    in
    for r := 0 to 10 do
        if 1 = 1 then r := 1; ()
    end",
            ),
            (
                "records same but separate decls are distinct types",
                r"let
    type any1 = {any: int}
    type any2 = {any: int}
    var a : any1 := nil
    var b : any2 := nil
    in
    a = b
    end",
            ),
            (
                "arrays same but separate decls are distinct types",
                r"let
    type arr1 = array of int
    type arr2 = array of int
    var a1 := arr1[8] of 0
    var a2 := arr2[8] of 0
    in
    a1 = a2
    end",
            ),
            (
                "break not in while/for fails",
                r"
    let
    in
        break
    end",
            ),
        ];
        for (_, input) in inputs {
            test_input_helper(input, false, None);
        }
    }

    #[test]
    fn rvalue_assignment_disallowed() {
        // variables, proc params, fields of records, and
        // array elements. functions can return record or array type, but those
        // would be references to some block of memory, and assigning to that
        // reference doesn't make the same sense as assigning to a var of
        // array/record type. for other types, you are just returning a value,
        // so it makes no sense to assign to those either.

        // luckily, it's built into the syntax so we don't have to worry about
        // that scenario.
        let inputs = [
            (
                "assignment to function call scalar result is invalid",
                r"
        let
            function x() : int =
                100
        in
            x() := 20
        end
        ",
            ),
            (
                "assignment to function call array result is invalid",
                r"
        let
            type intArray = array of int
            function x() : intArray =
                intArray[8] of 0
            var t := intArray[8] of 0
        in
            x() := t
        end
        ",
            ),
            (
                "assignment to function call record result is invalid",
                r"
        let
            type rec = {i : int}
            function x() : rec =
                rec{i=42}
            var t := rec{i=43}
        in
            x() := t
        end
        ",
            ),
        ];
        for (_, input) in inputs {
            let lexerdef = tiger_l::lexerdef();
            let lexer = lexerdef.lexer(&input);
            let (_res, errs) = tiger_y::parse(&lexer);
            assert!(errs.len() > 0);
        }
    }

    #[test]
    fn escape_simple() {
        let input = r"
    let
        var i := 1
        var x := 2
        function j () =
            i := 3
    in
        let
            function k() =
                (i := 4;
                x := 5)
        in
        end
    end
        ";
        let ast = test_input_helper(input, true, None);
        match &ast {
            Exp::LetExp { decs, .. } => match decs.as_slice() {
                [Dec::VarDec { escape: true, .. }, Dec::VarDec { escape: true, .. }, Dec::FunctionDec(..)] =>
                    {}
                _ => {
                    panic!("Unexpected AST structure for this program; ast={:#?}", ast);
                }
            },
            _ => {
                panic!("Unexpected AST structure for this program; ast={:#?}", ast);
            }
        }
    }

    #[test]
    fn escape_not_var_redeclared_as_function() {
        let input = r"
    let
        var i := 1
        function j () =
            i()
        function i () =
            ()
    in
    end
    ";
        let ast = test_input_helper(input, true, None);
        match &ast {
            Exp::LetExp { decs, .. } => match decs.as_slice() {
                [Dec::VarDec { escape: false, .. }, Dec::FunctionDec(..)] => {}
                _ => {
                    panic!("Unexpected AST structure for this program; ast={:#?}", ast);
                }
            },
            _ => {
                panic!("Unexpected AST structure for this program; ast={:#?}", ast);
            }
        }
    }

    #[test]
    fn escape_when_var_gets_redeclared() {
        let input = r"
    let
        var i := 1 /* get shadowed so shouldn't move */
        var i := 2 /* i escapes */
        function j () =
            i := 3
        var i := 4 /* this is fresh and shouldn't escape */
        var j := 5  /* gets used later */
    in
        let
            function x() =
                j := 42
        in
        end
    end
    ";
        let ast = test_input_helper(input, true, None);
        match &ast {
            Exp::LetExp { decs, .. } => match decs.as_slice() {
                [Dec::VarDec { escape: false, .. }, Dec::VarDec { escape: true, .. }, Dec::FunctionDec(..), Dec::VarDec { escape: false, .. }, Dec::VarDec { escape: true, .. }] =>
                    {}
                _ => {
                    panic!("Unexpected AST structure for this program; ast={:#?}", ast);
                }
            },
            _ => {
                panic!("Unexpected AST structure for this program; ast={:#?}", ast);
            }
        }
    }

    #[test]
    fn escape_for_variable() {
        let input = r"
    let
    in
        for i := 1 to 100
        do
            let
                function j () : int =
                    i + 1
            in
            end
    end
    ";
        let ast = test_input_helper(input, true, None);
        match &ast {
            Exp::LetExp { body, .. } => match body.as_ref() {
                Exp::SeqExp(exps) => match exps.as_slice() {
                    [Exp::ForExp { escape: true, .. }] => {}
                    _ => {
                        panic!("Unexpected AST structure for this program; ast={:#?}", ast);
                    }
                },
                _ => {
                    panic!("Unexpected AST structure for this program; ast={:#?}", ast);
                }
            },
            _ => {
                panic!("Unexpected AST structure for this program; ast={:#?}", ast);
                // test "escape vaGood source
            }
        }
    }

    #[test]
    fn escape_var_for_no_interaction() {
        let input = r"
    let
        var i:= 1
        in
        for i := 1 to 100
        do
            ()
    end
    ";
        let ast = test_input_helper(input, true, None);
        match &ast {
            Exp::LetExp { decs, .. } => {
                assert!(decs.len() == 1);
                match decs[0] {
                    Dec::VarDec { escape: false, .. } => {}
                    _ => {
                        panic!("Unexpected AST structure for this program; ast={:#?}", ast);
                    }
                }
            }
            _ => {
                panic!("Unexpected AST structure for this program; ast={:#?}", ast);
                // test "escape vaGood source
            }
        }
    }

    #[test]
    fn escape_not_for_loop_var_by_lo_hi() {
        let input = r"
    let
        var i := 0
        in
    for i := let function j() = i:= 1 in 1 end to let function j() = i:= 1 in 100 end
            do
                ()
    end
    ";
        let ast = test_input_helper(input, true, None);
        match &ast {
            Exp::LetExp { body, .. } => match body.as_ref() {
                Exp::SeqExp(exps) => {
                    assert!(exps.len() == 1);
                    match exps[0] {
                        Exp::ForExp { escape: false, .. } => {}
                        _ => {
                            panic!("Unexpected AST structure for this program; ast={:#?}", ast)
                        }
                    }
                }
                _ => panic!("Unexpected AST structure for this program; ast={:#?}", ast),
            },
            _ => {
                panic!("Unexpected AST structure for this program; ast={:#?}", ast);
            }
        }
    }
}
