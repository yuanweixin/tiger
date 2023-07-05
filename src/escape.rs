//  definition of escape in appel:
//  passed by reference (arrays, records and strings are heap allocated in tiger)
//  OR
//  address is taken (no address operator in tiger)
//  OR
//  accessed from a nested function
//
//
//  so, only way is to access a var from inside a nested function.
//
//  in plain english, escape just means "does this variable need to be allocated on the stack?". in tiger,
//  this is when a local variable is referenced in a nested function. That means in the general case, we would
//  have to keep the variable on the stack.
//
//  the impl is a recursive traversal of the ast using symbol table tracking the depth at which a var is declared.
//  then, any use at a larger depth means that var has escaped. the traversal is side-effectful, by modifying the `escape`
//  bool field in the relevant AST nodes.
//
//  impl is complicated by the redeclaration rules:
//  func/var name conflict is allowed as they live in same namespace.
//  recursive func decl block means func name that conflict with a var name will be effective
//  at the beginning of the func decl block.
//
//  an example
//  let
//   var i := 1
//   var i := 2 /* i above irrelevant for escape analysis */
//   var k := 3
//  absyn /* function i's scope effectively at start of recursive blk */
//   function j () =
//       i()
//       k + 1 /* k escapes */
//   function i () =  /* shadows var i */
//       2
//  in
//  end
//
use crate::{
    absyn::{Dec, Exp, Var},
    semant::TypeCheckingContext,
    symtab::SymbolTable,
    frame::Frame
};
use std::num::NonZeroUsize;

type DepthEscapeRef<'a> = (NonZeroUsize, &'a mut bool);
type EscapeEnv<'a> = SymbolTable<DepthEscapeRef<'a>>;

fn traverse_exp<'a, T: Frame>(
    ctx: &mut TypeCheckingContext<T>,
    env: &mut EscapeEnv<'a>,
    d: NonZeroUsize,
    exp: &'a mut Exp,
) {
    match exp {
        Exp::OpExp { left, right, .. } => {
            traverse_exp(ctx, env, d, left);
            traverse_exp(ctx, env, d, right);
        }
        Exp::VarExp(v) => {
            traverse_var(ctx, env, d, v);
        }
        Exp::CallExp { args, .. } => {
            for e in args {
                traverse_exp(ctx, env, d, e);
            }
        }
        Exp::RecordExp { fields, .. } => {
            for (_, e, _) in fields {
                traverse_exp(ctx, env, d, e);
            }
        }
        Exp::SeqExp(el) => {
            for e in el {
                traverse_exp(ctx, env, d, e);
            }
        }
        Exp::AssignExp { var, exp, .. } => {
            traverse_var(ctx, env, d, var);
            traverse_exp(ctx, env, d, exp);
        }
        Exp::IfExp {
            test, then, els, ..
        } => {
            traverse_exp(ctx, env, d, test);
            traverse_exp(ctx, env, d, then);
            if els.is_some() {
                traverse_exp(ctx, env, d, els.as_mut().unwrap());
            }
        }
        Exp::WhileExp { test, body, .. } => {
            traverse_exp(ctx, env, d, test);
            traverse_exp(ctx, env, d, body);
        }
        Exp::ForExp {
            var,
            lo,
            hi,
            body,
            escape,
            ..
        } => {
            env.begin_scope(); // loop counter only valid here.

            traverse_exp(ctx, env, d, lo);
            traverse_exp(ctx, env, d, hi);
            let var_sym = ctx.intern(var);
            env.enter(var_sym, (d, escape));

            traverse_exp(ctx, env, d, body);
            env.end_scope();
        }
        Exp::LetExp { decs, body, .. } => {
            env.begin_scope();

            for dec in decs {
                traverse_dec(ctx, env, d, dec);
            }
            traverse_exp(ctx, env, d, body);
            env.end_scope();
        }
        Exp::ArrayExp { size, init, .. } => {
            traverse_exp(ctx, env, d, size);
            traverse_exp(ctx, env, d, init);
        }
        // The rest of cases I explicitly list out so that in case the AST has new node types
        // the match statement will error out instead of failing silently.
        Exp::NilExp | Exp::StringExp(..) | Exp::BreakExp(..) | Exp::IntExp(..) => {}
    }
}

fn traverse_var<'a, T: Frame>(
    ctx: &mut TypeCheckingContext<T>,
    env: &mut EscapeEnv<'a>,
    cur_depth: NonZeroUsize,
    var: &'a mut Var,
) {
    match var {
        // Id
        Var::SimpleVar(span, ..) => {
            // because this stage happens before type checking
            // the var could refer to a non-existent variable.
            // hence the if check.
            let symbol = ctx.intern(span);
            let entry = env.look_mut(symbol);
            println!(
                "entry for {} is {:#?}",
                ctx.resolve_unchecked(&symbol),
                entry
            );
            if entry.is_some() {
                let e = entry.unwrap();
                if cur_depth > e.0 {
                    *e.1 = true;
                }
            }
        }
        // left.field
        Var::FieldVar(left, ..) => {
            traverse_var(ctx, env, cur_depth, left);
        }
        // Id [ exp ]
        Var::SubscriptVar(var, exp, ..) => {
            traverse_var(ctx, env, cur_depth, var);
            traverse_exp(ctx, env, cur_depth, exp);
        }
    }
}

fn traverse_dec<'a, T: Frame>(
    ctx: &mut TypeCheckingContext<T>,
    env: &mut EscapeEnv<'a>,
    depth: NonZeroUsize,
    dec: &'a mut Dec,
) {
    match dec {
        Dec::TypeDec(..) => {}
        Dec::FunctionDec(v) => {
            let new_depth = depth.checked_add(1).unwrap();
            for fundec in v {
                for field in &mut fundec.params {
                    let sym = ctx.intern(&field.name);
                    env.enter(sym, (new_depth, &mut field.escape));
                }
                traverse_exp(ctx, env, new_depth, &mut fundec.body);
            }
        }
        Dec::VarDec { name, escape, .. } => {
            let sym = ctx.intern(name);
            env.enter(sym, (depth, escape));
        }
    }
}

pub fn find_escapes<T: Frame>(ctx: &mut TypeCheckingContext<T>, prog: &mut Exp) {
    let mut env = EscapeEnv::empty();
    env.begin_scope();
    let d = NonZeroUsize::MIN;
    traverse_exp(ctx, &mut env, d, prog);
}
