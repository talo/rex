use std::{borrow::Borrow, collections::BTreeMap, sync::Arc};

use futures::future;
use rex_ast::{
    expr::{Expr, Scope, Var},
    id::Id,
};
use rex_lexer::span::Span;
use rex_type_system::{
    types::{ExprTypeEnv, Type},
    unify::{self, Subst},
};
use tokio::sync::RwLock;

use crate::{error::Error, ftable::Ftable};

#[derive(Clone)]
pub struct Context<State>
where
    State: Clone + Sync + 'static,
{
    pub scope: Scope,
    pub ftable: Ftable<State>,
    pub subst: Subst,
    pub env: Arc<RwLock<ExprTypeEnv>>,
    pub state: State,
}

#[async_recursion::async_recursion]
pub async fn eval<State>(ctx: &Context<State>, expr: &Expr) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    match expr {
        Expr::Bool(..) |
        Expr::Uint(..) |
        Expr::Int(..) |
        Expr::Float(..) |
        Expr::String(..) |
        Expr::Named(..) => {
            Ok(expr.clone())
        }
        Expr::Tuple(id, span, tuple) => eval_tuple(ctx, id, span, tuple).await,
        Expr::List(id, span, list) => eval_list(ctx, id, span, list).await,
        Expr::Dict(id, span, dict) => eval_dict(ctx, id, span, dict).await,
        Expr::Var(var) => eval_var(ctx, var).await,
        Expr::App(id, span, f, x) => eval_app(ctx, id, span, f, x).await,
        Expr::Lam(id, span, scope, param, body) => {
            eval_lam(ctx, id, span, scope, param, body).await
        }
        Expr::Let(id, span, var, def, body) => eval_let(ctx, id, span, var, def, body).await,
        Expr::Ite(id, span, cond, then, r#else) => {
            eval_ite(ctx, id, span, cond, then, r#else).await
        }
        Expr::Curry(id, span, f, args) => Ok(Expr::Curry(
            id.clone(),
            span.clone(),
            f.clone(),
            args.clone(),
        )),
    }
}

pub async fn eval_tuple<State>(
    ctx: &Context<State>,
    id: &Id,
    span: &Span,
    tuple: &Vec<Expr>,
) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    let mut result = Vec::with_capacity(tuple.len());
    for v in tuple {
        result.push(eval(ctx, v));
    }
    Ok(Expr::Tuple(
        id.clone(),
        span.clone(),
        future::join_all(result)
            .await
            .into_iter()
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

pub async fn eval_list<State>(
    ctx: &Context<State>,
    id: &Id,
    span: &Span,
    list: &Vec<Expr>,
) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    let mut result = Vec::with_capacity(list.len());
    for v in list {
        result.push(eval(ctx, v));
    }
    Ok(Expr::List(
        id.clone(),
        span.clone(),
        future::join_all(result)
            .await
            .into_iter()
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

pub async fn eval_dict<State>(
    ctx: &Context<State>,
    id: &Id,
    span: &Span,
    dict: &BTreeMap<String, Expr>,
) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    let mut keys = Vec::with_capacity(dict.len());
    let mut vals = Vec::with_capacity(dict.len());
    for (k, v) in dict {
        keys.push(k);
        vals.push(eval(ctx, v));
    }

    let mut result = BTreeMap::new();
    for (k, v) in keys.into_iter().zip(future::join_all(vals).await) {
        result.insert(k.clone(), v?);
    }
    Ok(Expr::Dict(id.clone(), span.clone(), result))
}

#[async_recursion::async_recursion]
pub async fn eval_var<State>(ctx: &Context<State>, var: &Var) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    let var_type = unify::apply_subst(ctx.env.read().await.get(&var.id).unwrap(), &ctx.subst);
    print!("evaluating var: ({}):({})...", var, var_type);

    if let Some(expr) = ctx.scope.get(&var.name) {
        let expr_type =
            unify::apply_subst(ctx.env.read().await.get(expr.id()).unwrap(), &ctx.subst);
        println!(" found in scope: {}:{}", expr, expr_type);

        let mut new_expr = expr.clone();
        *new_expr.id_mut() = Id::new();
        ctx.env
            .write()
            .await
            .insert(*new_expr.id(), var_type.clone());

        let new_expr = if let Expr::Bool(..)
        | Expr::Uint(..)
        | Expr::Int(..)
        | Expr::Float(..)
        | Expr::String(..) = new_expr
        {
            // We have arrived at a concrete value
            new_expr.clone()
        } else {
            println!(" overriding with type: {}", var_type);
            // We have not arrived at a concrete value
            eval(ctx, &new_expr).await?
        };

        return Ok(new_expr.clone());
    }

    let var = match ctx.scope.get(&var.name) {
        None => var,
        Some(Expr::Var(var)) => return eval_var(ctx, var).await,
        Some(non_var_expr) => return eval(ctx, non_var_expr).await, // Ok(non_var_expr.clone()),
    };

    let f = ctx.ftable.lookup_fns(&var.name, var_type.clone()).next();
    if let Some((f, f_type)) = f {
        if f_type.num_params() == 0 {
            let res = f(ctx, &vec![]).await?;
            ctx.env.write().await.insert(*res.id(), var_type.clone());
            println!(
                " found in ftable: {} and evaluated to: ({}):({})",
                f_type, res, var_type
            );
            return Ok(res);
        }
        println!(" found in ftable: {}", var_type);

        return Ok(Expr::Var(var.clone()));
    }

    println!(" not found in ftable!");
    Err(Error::VarNotFound { var: var.clone() })
}

pub async fn eval_app<State>(
    ctx: &Context<State>,
    id: &Id,
    _span: &Span,
    f: &Expr,
    x: &Expr,
) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    let fx_type = unify::apply_subst(ctx.env.read().await.get(id).unwrap(), &ctx.subst);
    println!("applying: ({} {}): {}", f, x, fx_type);
    apply(ctx, f, x).await
}

pub async fn apply<State>(
    ctx: &Context<State>,
    f: impl Borrow<Expr>,
    x: impl Borrow<Expr>,
) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    let f_type: Type = unify::apply_subst(
        ctx.env.read().await.get(f.borrow().id()).unwrap(),
        &ctx.subst,
    );
    let x_type: Type = unify::apply_subst(
        ctx.env.read().await.get(x.borrow().id()).unwrap(),
        &ctx.subst,
    );
    let (_, b_type) = match &f_type {
        Type::Arrow(a, b) => (a.clone(), b.clone()),
        _ => panic!("Function application on non-function type"),
    };

    let f = eval(ctx, f.borrow()).await?;
    let x = eval(ctx, x.borrow()).await?;

    let res = match f {
        Expr::Var(var) => {
            // TODO(loong): we should be checking if more than one function is
            // found. This is an ambiguity error.
            let f = ctx.ftable.lookup_fns(&var.name, f_type.clone()).next();
            if let Some((f, _ftype)) = f {
                match f_type.num_params() {
                    0 => panic!("Function application on non-function type"),
                    1 => {
                        println!(
                            "calling function: ({}:{}) ({}:{}) results in type: {}",
                            &var, &f_type, &x, &x_type, &b_type
                        );
                        f(ctx, &vec![x]).await?
                    }
                    // TODO(loong): fix the span.
                    _ => {
                        println!(
                            "creating curry: ({}:{}) ({}:{})",
                            &var, &f_type, &x, &x_type
                        );

                        let mut x = x.clone();
                        *x.id_mut() = Id::new();
                        ctx.env.write().await.insert(*x.id(), x_type.clone());

                        // NOTE(loong): functions in the ftable having explicit
                        // ids would probably be very helpful. Right now,
                        // relying on the var that pointed to the function in
                        // the ftable makes things a little hard to understand.
                        Expr::Curry(Id::new(), var.span, var, vec![x])
                    }
                }
            } else {
                panic!("Function not found: {}:{}", var.name, f_type)
            }
        }
        Expr::Lam(_id, _span, scope, param, body) => {
            let new_body_id = Id::new();
            let mut body = match *body {
                Expr::App(_, span, g, y) => {
                    let new_g_id = Id::new();
                    let new_y_id = Id::new();

                    let mut g = g.clone();
                    *g.id_mut() = new_g_id;
                    ctx.env.write().await.insert(new_g_id, f_type.clone());

                    let mut y = y.clone();
                    *y.id_mut() = new_y_id;
                    ctx.env.write().await.insert(new_y_id, x_type.clone());

                    Expr::App(new_body_id, span.clone(), g, y)
                }
                body => body,
            };
            *body.id_mut() = new_body_id;
            ctx.env.write().await.insert(new_body_id, *b_type.clone());

            let mut ctx: Context<State> = ctx.clone();
            ctx.scope.insert_mut(param.name, x);
            for (k, v) in scope.iter() {
                ctx.scope.insert_mut(k.clone(), v.clone());
            }

            eval(&ctx, &body).await?
        }
        Expr::Curry(_id, span, var, mut args) => {
            let f_type = unify::apply_subst(ctx.env.read().await.get(&var.id).unwrap(), &ctx.subst);

            // FIXME(loong): to fix the `test_f_passthrough` test it is pretty
            // clear to me that we need to differentiate between functions that
            // return functions, and functions that take in multiple arguments.
            // This is *only* different for curried functions. The issue with
            // `test_f_passthrough` is that because `id (&&)` returns `bool ->
            // bool -> bool` this logic will not call `id` until all the bool
            // arguments are also pushed (because the type of `id` is known to
            // be the type of `&&`) but the actual implementation of `id`
            // ignores the `bool` arguments and they are therefore lost.
            //
            // The solution is to alter the way a curried function is actually
            // called by the `ftable` and it loops until all arguments are
            // consumed.
            println!(
                "pushing to args: {}[{}] arg: {}",
                var,
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                x
            );
            args.push(x.clone());
            if f_type.num_params() < args.len() {
                panic!("Too many arguments");
            } else if f_type.num_params() == args.len() {
                let mut args_fmt = Vec::new();
                let mut arg_types = Vec::new();
                for arg in &args {
                    args_fmt.push(format!(
                        "({})",
                        unify::apply_subst(ctx.env.read().await.get(arg.id()).unwrap(), &ctx.subst,)
                    ));
                    arg_types.push(unify::apply_subst(
                        ctx.env.read().await.get(arg.id()).unwrap(),
                        &ctx.subst,
                    ));
                }

                println!(
                    "curried function lookup: ({}:{}) {} -> ({})",
                    &var,
                    &f_type,
                    &args_fmt.join(" -> "),
                    &b_type
                );

                let f_type = Type::build_arrow(arg_types, *b_type.clone());

                // TODO(loong): we should be checking if more than one function is
                // found. This is an ambiguity error.
                let f = ctx.ftable.lookup_fns(&var.name, f_type.clone()).next();

                if let Some((f, found_ftype)) = f {
                    println!("  ↳curried function found: {}", found_ftype);
                    println!(
                        "  ↳calling curried function: ({}:{}) ({}:{}) results in type: {}",
                        &var, &f_type, &x, &x_type, &b_type
                    );
                    let res = f(ctx, &args).await?;

                    println!("  ↳curry function result: {}", &res);
                    res
                } else {
                    panic!("Function not found: {}:{}", var.name, f_type)
                }
            } else {
                // TODO(loong): fix the span.
                Expr::Curry(Id::new(), span, var, args)
            }
        }
        _ => unimplemented!(),
    };

    ctx.env.write().await.insert(*res.id(), *b_type);

    Ok(res)
}

pub async fn eval_lam<State>(
    ctx: &Context<State>,
    id: &Id,
    span: &Span,
    scope: &Scope,
    param: &Var,
    body: &Expr,
) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    let mut scope = scope.clone();
    for entry in ctx.scope.iter() {
        scope.insert_mut(entry.0.clone(), entry.1.clone());
    }
    Ok(Expr::Lam(
        id.clone(),
        span.clone(),
        scope,
        param.clone(),
        Box::new(body.clone()),
    ))
}

pub async fn eval_let<State>(
    ctx: &Context<State>,
    _id: &Id,
    _span: &Span,
    var: &Var,
    def: &Expr,
    body: &Expr,
) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    // let def = eval(ctx, def).await?;
    let mut ctx = ctx.clone();
    // NOTE(loong): this is lazy evaluation. We do not compute the definition of
    // the binding. This is important because it means we do not have to know
    // all the types until the binding is actually used, and that's where we
    // will actually have the types available.
    ctx.scope = ctx.scope.insert(var.name.clone(), def.clone());
    eval(&ctx, body).await
}

pub async fn eval_ite<State>(
    ctx: &Context<State>,
    _id: &Id,
    _span: &Span,
    cond: &Expr,
    then: &Expr,
    r#else: &Expr,
) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    let cond = eval(ctx, cond).await?;
    match cond {
        Expr::Bool(_, _, true) => eval(ctx, then).await,
        Expr::Bool(_, _, false) => eval(ctx, r#else).await,
        _ => unimplemented!(),
    }
}

#[cfg(test)]
pub mod test {
    use rex_ast::{assert_expr_eq, b, d, f, i, l, s, tup, u, n};
    use rex_lexer::Token;
    use rex_parser::Parser;
    use rex_type_system::{
        bool,
        constraint::generate_constraints,
        dict, float, int, list, string,
        trace::sprint_expr_with_type,
        tuple,
        types::Type,
        uint,
        result,
        option,
        unify::{self},
    };

    use crate::engine::Builder;

    use super::*;

    /// Helper function for parsing, inferring, and evaluating a given code
    /// snippet. Pretty much all of the test suites can use this flow for
    /// testing that the engine is correctly evaluating types and expressions.
    /// In the future, we should probably make this (or some version of this) an
    /// actual function for library users too.
    async fn parse_infer_and_eval(code: &str) -> Result<(Expr, Type), Error> {
        let mut parser = Parser::new(Token::tokenize(code).unwrap());
        let expr = parser.parse_expr().unwrap();

        let builder = Builder::with_prelude().unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(&expr, &type_env, &mut expr_type_env, &mut constraint_system)
            .unwrap();

        let subst = unify::unify_constraints(&constraint_system).unwrap();
        let res_type = unify::apply_subst(&ty, &subst);

        println!(
            "{}\n",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst))
        );

        let res = eval(
            &Context {
                scope: Scope::new_sync(),
                ftable,
                subst,
                env: Arc::new(RwLock::new(expr_type_env)),
                state: (),
            },
            &expr,
        )
        .await;

        res.map(|res| (res, res_type))
    }

    #[tokio::test]
    async fn test_literals() {
        let (res, res_type) = parse_infer_and_eval(r#"true"#).await.unwrap();
        assert_eq!(res_type, bool!());
        assert_expr_eq!(res, b!(true); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"420"#).await.unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(420); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"6.9"#).await.unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(6.9); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#""Hello, world!""#).await.unwrap();
        assert_eq!(res_type, string!());
        assert_expr_eq!(res, s!("Hello, world!"); ignore span);
    }

    #[tokio::test]
    async fn test_negate() {
        let (res, res_type) = parse_infer_and_eval(r#"-420"#).await.unwrap();
        assert_eq!(res_type, int!());
        assert_expr_eq!(res, i!(-420); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"-3.14"#).await.unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(-3.14); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"(-3.14, -69)"#).await.unwrap();
        assert_eq!(res_type, tuple!(float!(), int!()));
        assert_expr_eq!(res, tup!(f!(-3.14), i!(-69)); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"[-0, -314, -69, -420]"#)
            .await
            .unwrap();
        assert_eq!(res_type, list!(int!()));
        assert_expr_eq!(res, l!(i!(0), i!(-314), i!(-69), i!(-420)); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"[-0.0, -3.14, -6.9, -42.0]"#)
            .await
            .unwrap();
        assert_eq!(res_type, list!(float!()));
        assert_expr_eq!(res, l!(f!(-0.0), f!(-3.14), f!(-6.9), f!(-42.0)); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"{ x = -3.14, y = -69 }"#)
            .await
            .unwrap();
        assert_eq!(res_type, dict! { x: float!(), y: int!() });
        assert_expr_eq!(res, d!{ x = f!(-3.14), y = i!(-69)}; ignore span);
    }

    #[tokio::test]
    async fn test_maths() {
        let (res, res_type) = parse_infer_and_eval(r#"6.9 + 4.20"#).await.unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(11.100000000000001); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"6 + 9"#).await.unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(15); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"(-4) + (-20)"#).await.unwrap();
        assert_eq!(res_type, int!());
        assert_expr_eq!(res, i!(-24); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"(-4) + (int 20)"#).await.unwrap();
        assert_eq!(res_type, int!());
        assert_expr_eq!(res, i!(16); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"6.9 - 4.20"#).await.unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(2.7); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"(int 6) - (int 9)"#).await.unwrap();
        assert_eq!(res_type, int!());
        assert_expr_eq!(res, i!(-3); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"(-) (int 4) (int 20)"#)
            .await
            .unwrap();
        assert_eq!(res_type, int!());
        assert_expr_eq!(res, i!(-16); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"4 * 20 + 6 * 9"#).await.unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(134); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"4 * (20 + 6) * 9"#).await.unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(936); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"((+) 3 14) * 4 + ((*) 20 6) / 9"#)
            .await
            .unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(81); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"((+) 3.0 1.4) * 4.0 + ((*) 2.0 6.9) / 3.14"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(21.99490445859873); ignore span);
    }

    #[tokio::test]
    async fn test_let_add_in_add() {
        let (res, res_type) = parse_infer_and_eval(r#"let f = λx → x + x in f (6.9 + 3.14)"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(20.080000000000002); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let f = λx → x + x in f (id 6.9 + id 3.14)"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(20.080000000000002); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let f = λx → x + x in f (id (6.9 + 3.14))"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(20.080000000000002); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let f = λx → x + x in id (f (6.9 + 3.14))"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(20.080000000000002); ignore span);

        let (res, res_type) =
            parse_infer_and_eval(r#"let f = λx → (id x + id x) in f (6.9 + 3.14)"#)
                .await
                .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(20.080000000000002); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let f = λx → id (x + x) in f (6.9 + 3.14)"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(20.080000000000002); ignore span);
    }

    #[tokio::test]
    async fn test_let_id_in() {
        let (res, res_type) = parse_infer_and_eval(r#"let f = id in id 420"#)
            .await
            .unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(420); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let f = id in id 6.9"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(6.9); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let f = id in id 69"#)
            .await
            .unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(69); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let f = id in id 3.14"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(3.14); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let f = id in (f 6.9) + (f 3.14)"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(10.040000000000001); ignore span);
    }

    #[tokio::test]
    async fn test_tuple() {
        let (res, res_type) = parse_infer_and_eval(r#"(6.9, 420, true)"#).await.unwrap();
        assert_eq!(res_type, tuple!(float!(), uint!(), bool!()));
        assert_expr_eq!(res, tup!(f!(6.9), u!(420), b!(true)); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"(6.9, 420, true, )"#).await.unwrap();
        assert_eq!(res_type, tuple!(float!(), uint!(), bool!()));
        assert_expr_eq!(res, tup!(f!(6.9), u!(420), b!(true)); ignore span);

        let (res, res_type) =
            parse_infer_and_eval(r#"(3.14 * 6.9, 20 * 4, (*) (int 4) (int 105), true || false)"#)
                .await
                .unwrap();
        assert_eq!(res_type, tuple!(float!(), uint!(), int!(), bool!()));
        assert_expr_eq!(res, tup!(f!(21.666), u!(80), i!(420), b!(true)); ignore span);

        let (res, res_type) = parse_infer_and_eval(
            r#"((id 3.14) * (id 6.9), (id 20) * (id 4), (*) (id (int 4)) (id (int 105)), (id true) || (id false))"#,
        )
        .await
        .unwrap();
        assert_eq!(res_type, tuple!(float!(), uint!(), int!(), bool!()));
        assert_expr_eq!(res, tup!(f!(21.666), u!(80), i!(420), b!(true)); ignore span);

        let (res, res_type) = parse_infer_and_eval(
            r#"(id (3.14 * 6.9), id (20 * 4), id ((*) (int 4) (int 105)), id (true || false))"#,
        )
        .await
        .unwrap();
        assert_eq!(res_type, tuple!(float!(), uint!(), int!(), bool!()));
        assert_expr_eq!(res, tup!(f!(21.666), u!(80), i!(420), b!(true)); ignore span);

        let (res, res_type) = parse_infer_and_eval(
            r#"(id ((id 3.14) * (id 6.9)), id ((id 20) * (id 4)), id ((*) (id (int 4)) (id (int 105))), id ((id true) || (id false)))"#,
        )
        .await
        .unwrap();
        assert_eq!(res_type, tuple!(float!(), uint!(), int!(), bool!()));
        assert_expr_eq!(res, tup!(f!(21.666), u!(80), i!(420), b!(true)); ignore span);
    }

    #[tokio::test]
    async fn test_list() {
        // Empty list
        let (res, res_type) = parse_infer_and_eval(r#"[]"#).await.unwrap();
        assert!(match res_type {
            Type::List(inner) => matches!(&*inner, Type::Var(_)),
            _ => false,
        });
        assert_expr_eq!(res, l!(); ignore span);

        // Single item
        let (res, res_type) = parse_infer_and_eval(r#"[420]"#).await.unwrap();
        assert_eq!(res_type, list!(uint!()));
        assert_expr_eq!(res, l!(u!(420)); ignore span);

        // Single item with trailing comma
        let (res, res_type) = parse_infer_and_eval(r#"[420,]"#).await.unwrap();
        assert_eq!(res_type, list!(uint!()));
        assert_expr_eq!(res, l!(u!(420)); ignore span);

        // Multiple items
        let (res, res_type) = parse_infer_and_eval(r#"[420, 69, 555]"#).await.unwrap();
        assert_eq!(res_type, list!(uint!()));
        assert_expr_eq!(res, l!(u!(420), u!(69), u!(555)); ignore span);

        // Multiple items with trailing comma
        let (res, res_type) = parse_infer_and_eval(r#"[420, 69, 555,]"#).await.unwrap();
        assert_eq!(res_type, list!(uint!()));
        assert_expr_eq!(res, l!(u!(420), u!(69), u!(555)); ignore span);
    }

    #[tokio::test]
    async fn test_dict() {
        // Empty dictionary
        let (res, res_type) = parse_infer_and_eval(r#"{}"#).await.unwrap();
        assert_eq!(res_type, dict! { });
        assert_expr_eq!(res, d!(); ignore span);

        // Single item
        let (res, res_type) = parse_infer_and_eval(r#"{ a = 420 }"#).await.unwrap();
        assert_eq!(res_type, dict! { a: uint!() });
        assert_expr_eq!(res, d!(a = u!(420)); ignore span);

        // Single item with trailing comma
        let (res, res_type) = parse_infer_and_eval(r#"{ a = 420, }"#).await.unwrap();
        assert_eq!(res_type, dict! { a: uint!() });
        assert_expr_eq!(res, d!( a = u!(420)); ignore span);

        // Multiple items
        let (res, res_type) = parse_infer_and_eval(r#"{ a = 420, b = 3.14, c = "hello" }"#).await.unwrap();
        assert_eq!(res_type, dict! { a: uint!(), b: float!(), c: string!() });
        assert_expr_eq!(res, d!(a = u!(420), b = f!(3.14), c = s!("hello")); ignore span);

        // Multiple items with trailing comma
        let (res, res_type) = parse_infer_and_eval(r#"{ a = 420, b = 3.14, c = "hello", }"#).await.unwrap();
        assert_eq!(res_type, dict! { a: uint!(), b: float!(), c: string!() });
        assert_expr_eq!(res, d!(a = u!(420), b = f!(3.14), c = s!("hello")); ignore span);
    }

    #[tokio::test]
    async fn test_f_passthrough() {
        let (res, res_type) = parse_infer_and_eval(r#"(id (&&)) true true"#)
            .await
            .unwrap();
        assert_eq!(res_type, bool!());
        assert_expr_eq!(res, b!(true); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"(id (+)) 69 420"#).await.unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(489); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"(id (+)) 6.9 42.0"#).await.unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(48.9); ignore span);
    }

    #[tokio::test]
    async fn test_polymorphism() {
        let (res, res_type) = parse_infer_and_eval(r#"id (id 6.9, id 420, id (-420))"#)
            .await
            .unwrap();
        assert_eq!(res_type, tuple!(float!(), uint!(), int!()));
        assert_expr_eq!(res, tup!(f!(6.9), u!(420), i!(-420)); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"(6.9 + zero, 420 + zero, (-420) + zero)"#)
            .await
            .unwrap();
        assert_eq!(res_type, tuple!(float!(), uint!(), int!()));
        assert_expr_eq!(res, tup!(f!(6.9), u!(420), i!(-420)); ignore span);

        let (res, res_type) =
            parse_infer_and_eval(r#"swap (swap (6.9, 420), swap (true, "hello, world!"))"#)
                .await
                .unwrap();
        assert_eq!(
            res_type,
            tuple!(tuple!(string!(), bool!()), tuple!(uint!(), float!())),
        );
        assert_expr_eq!(res, tup!(tup!(s!("hello, world!"), b!(true)), tup!(u!(420), f!(6.9))); ignore span);
    }

    #[tokio::test]
    async fn test_let_polymorphism() {
        let (res, res_type) =
            parse_infer_and_eval(r#"let f = λx → x in (f 6.9, f 420, f true, f "hello, world!")"#)
                .await
                .unwrap();
        assert_eq!(res_type, tuple!(float!(), uint!(), bool!(), string!()),);
        assert_expr_eq!(
            res,
            tup!(f!(6.9), u!(420), b!(true), s!("hello, world!")); ignore span
        );
    }

    #[tokio::test]
    async fn test_let_parametric_polymorphism() {
        let (res, res_type) =
            parse_infer_and_eval(r#"let f = id in (f 6.9, f 420, f true, f "hello, world!")"#)
                .await
                .unwrap();
        assert_eq!(res_type, tuple!(float!(), uint!(), bool!(), string!()),);
        assert_expr_eq!(
            res,
            tup!(f!(6.9), u!(420), b!(true), s!("hello, world!")); ignore span
        );
    }

    #[tokio::test]
    async fn test_let_overloaded_polymorphism() {
        let (res, res_type) =
            parse_infer_and_eval(r#"let f = λx → -x in (f 6.9, f 420, f (int 314))"#)
                .await
                .unwrap();
        assert_eq!(res_type, tuple!(float!(), int!(), int!()));
        assert_expr_eq!(res, tup!(f!(-6.9), i!(-420), i!(-314)); ignore span);
    }

    #[tokio::test]
    async fn test_parametric_overloaded_let_polymorphism() {
        let (res, res_type) =
            parse_infer_and_eval(r#"let f = λx → id (x + x) in (f 6.9, f 420, f (-314))"#)
                .await
                .unwrap();
        assert_eq!(res_type, tuple!(float!(), uint!(), int!()));
        assert_expr_eq!(res, tup!(f!(13.8), u!(840), i!(-628)); ignore span);
    }

    #[tokio::test]
    async fn test_let_bind_to_ftable() {
        let (res, res_type) = parse_infer_and_eval(r#"let f = negate in f 420"#)
            .await
            .unwrap();
        assert_eq!(res_type, int!());
        assert_expr_eq!(res, i!(-420); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let f = negate in f 6.9"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(-6.9); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let x = zero in (420 + x)"#)
            .await
            .unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(420); ignore span);

        // FIXME(loong): this test is failing.
        let (res, res_type) = parse_infer_and_eval(r#"let x = zero in (6.9 + x)"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(6.9); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let f = (+) in f 69 420"#)
            .await
            .unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(489); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"let f = (+) in f 6.9 42.0"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(48.9); ignore span);
    }

    #[tokio::test]
    async fn test_let_in() {
        let (res, res_type) =
            parse_infer_and_eval(r#"let a = 420 in a"#)
                .await
                .unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(420); ignore span);

        let (res, res_type) =
            parse_infer_and_eval(r#"let a = 420, in a"#)
                .await
                .unwrap();
        assert_eq!(res_type, uint!());
        assert_expr_eq!(res, u!(420); ignore span);

        let (res, res_type) =
            parse_infer_and_eval(r#"let a = 420, b = 69 in (420, 69)"#)
                .await
                .unwrap();
        assert_eq!(res_type, tuple!(uint!(), uint!()));
        assert_expr_eq!(res, tup!(u!(420), u!(69)); ignore span);

        let (res, res_type) =
            parse_infer_and_eval(r#"let a = 420, b = 69, in (420, 69)"#)
                .await
                .unwrap();
        assert_eq!(res_type, tuple!(uint!(), uint!()));
        assert_expr_eq!(res, tup!(u!(420), u!(69)); ignore span);
    }

    #[tokio::test]
    async fn test_let_in_cascade() {
        let (res, res_type) =
            parse_infer_and_eval(r#"let f = (λx → -x), u = f 6.9, v = f u in (u, v)"#)
                .await
                .unwrap();
        assert_eq!(res_type, tuple!(float!(), float!()));
        assert_expr_eq!(res, tup!(f!(-6.9), f!(6.9)); ignore span);
    }

    #[tokio::test]
    async fn test_result() {
        let (res, res_type) =
            parse_infer_and_eval(r#"let a = Ok 4, b = Err "bad" in [a, b]"#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(result!(uint!(), string!())));
        assert_expr_eq!(res, l!(n!("Ok", Some(u!(4))), n!("Err", Some(s!("bad")))); ignore span);
    }

    #[tokio::test]
    async fn test_map_result() {
        let (res, res_type) =
            parse_infer_and_eval(r#"
                let
                    a = Ok 4,
                    b = Err "bad",
                    f = map_result (\x -> [x, x + 1, x + 2])
                in
                    map f [a, b]
                "#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(result!(list!(uint!()), string!())));
        assert_expr_eq!(
            res,
            l!(n!("Ok", Some(l!(u!(4), u!(5), u!(6)))),
               n!("Err", Some(s!("bad"))));
            ignore span);
    }

    #[tokio::test]
    async fn test_and_then_result() {
        let (res, res_type) =
            parse_infer_and_eval(r#"
                let
                    a = Ok 0,
                    b = Ok 1,
                    c = Err "bad",
                    f = and_then_result (\x -> if x == 0 then Ok 3.14 else Err "nonzero")
                in
                    map f [a, b, c]
                "#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(result!(float!(), string!())));
        assert_expr_eq!(
            res,
            l!(
                n!("Ok", Some(f!(3.14))),
                n!("Err", Some(s!("nonzero"))),
                n!("Err", Some(s!("bad"))));
            ignore span);
    }

    #[tokio::test]
    async fn test_or_else_result() {
        let (res, res_type) =
            parse_infer_and_eval(r#"
                let
                    a = Ok "one",
                    b = Err 0,
                    c = Err 1,
                    f = or_else_result (\x -> if x == 0 then Ok "yes" else Err 3.14)
                in
                    map f [a, b, c]
                "#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(result!(string!(), float!())));
        assert_expr_eq!(
            res,
            l!(
                n!("Ok", Some(s!("one"))),
                n!("Ok", Some(s!("yes"))),
                n!("Err", Some(f!(3.14))));
            ignore span);
    }

    #[tokio::test]
    async fn test_unwrap_or_else_result() {
        let (res, res_type) =
            parse_infer_and_eval(r#"
                let
                    a = Ok 4,
                    b = Err "bad",
                    f = unwrap_or_else_result (\x -> 99)
                in
                    map f [a, b]
                "#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(uint!()));
        assert_expr_eq!(res, l!(u!(4), u!(99)); ignore span);
    }

    #[tokio::test]
    async fn test_option() {
        let (res, res_type) =
            parse_infer_and_eval(r#"let a = Some 4, b = None in [a, b]"#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(option!(uint!())));
        assert_expr_eq!(res, l!(n!("Some", Some(u!(4))), n!("None", None)); ignore span);
    }

    #[tokio::test]
    async fn test_map_option() {
        let (res, res_type) =
            parse_infer_and_eval(r#"
                let
                    a = Some 4,
                    b = None,
                    f = map_option (\x -> [x, x + 1, x + 2])
                in
                    map f [a, b]
                "#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(option!(list!(uint!()))));
        assert_expr_eq!(
            res,
            l!(n!("Some", Some(l!(u!(4), u!(5), u!(6)))),
               n!("None", None));
            ignore span);
    }

    #[tokio::test]
    async fn test_and_then_option() {
        let (res, res_type) =
            parse_infer_and_eval(r#"
                let
                    a = Some 0,
                    b = Some 1,
                    c = None,
                    f = and_then_option (\x -> if x == 0 then Some 3.14 else None)
                in
                    map f [a, b, c]
                "#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(option!(float!())));
        assert_expr_eq!(
            res,
            l!(
                n!("Some", Some(f!(3.14))),
                n!("None", None),
                n!("None", None));
            ignore span);
    }

    #[tokio::test]
    async fn test_or_else_option() {
        let (res, res_type) =
            parse_infer_and_eval(r#"
                let
                    a = Some 5.1,
                    b = None,
                    f = or_else_option (\x -> Some 3.14)
                in
                    map f [a, b]
                "#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(option!(float!())));
        assert_expr_eq!(
            res,
            l!(
                n!("Some", Some(f!(5.1))),
                n!("Some", Some(f!(3.14))));
            ignore span);

        let (res, res_type) =
            parse_infer_and_eval(r#"
                let
                    a = Some 5.1,
                    b = None,
                    f = or_else_option (\x -> None)
                in
                    map f [a, b]
                "#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(option!(float!())));
        assert_expr_eq!(
            res,
            l!(
                n!("Some", Some(f!(5.1))),
                n!("None", None));
            ignore span);
    }

    #[tokio::test]
    async fn test_unwrap_or_else_option() {
        let (res, res_type) =
            parse_infer_and_eval(r#"
                let
                    a = Some 4,
                    b = None,
                    f = unwrap_or_else_option (\x -> 99)
                in
                    map f [a, b]
                "#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(uint!()));
        assert_expr_eq!(res, l!(u!(4), u!(99)); ignore span);
    }

    #[tokio::test]
    async fn test_map() {
        let (res, res_type) = parse_infer_and_eval(r#"map (λx → -x) [3.14, 6.9, 42.0, 1.0]"#)
            .await
            .unwrap();
        assert_eq!(res_type, list!(float!()));
        assert_expr_eq!(res, l!(f!(-3.14), f!(-6.9), f!(-42.0), f!(-1.0)); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"map id [3.14, 6.9, 42.0, 1.0]"#)
            .await
            .unwrap();
        assert_eq!(res_type, list!(float!()));
        assert_expr_eq!(res, l!(f!(3.14), f!(6.9), f!(42.0), f!(1.0)); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"map negate [3.14, 6.9, 42.0, 1.0]"#)
            .await
            .unwrap();
        assert_eq!(res_type, list!(float!()));
        assert_expr_eq!(res, l!(f!(-3.14), f!(-6.9), f!(-42.0), f!(-1.0)); ignore span);

        let (res, res_type) =
            parse_infer_and_eval(r#"map (let f = id in f) [3.14, 6.9, 42.0, 1.0]"#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(float!()));
        assert_expr_eq!(res, l!(f!(3.14), f!(6.9), f!(42.0), f!(1.0)); ignore span);

        let (res, res_type) =
            parse_infer_and_eval(r#"map (let f = (λx → -x) in f) [3.14, 6.9, 42.0, 1.0]"#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(float!()));
        assert_expr_eq!(res, l!(f!(-3.14), f!(-6.9), f!(-42.0), f!(-1.0)); ignore span);

        // FIXME(loong): this test is not passing.
        // ```rex
        // let (res, res_type) =
        //     parse_infer_and_eval(r#"map (let f = (λx → id (-x)) in f) [3.14, 6.9, 42.0, 1.0]"#)
        //         .await
        //         .unwrap();
        // assert_eq!(res_type, list!(float!()));
        // assert_expr_eq!(res, l!(f!(-3.14), f!(-6.9), f!(-42.0), f!(-1.0)); ignore span);
        // ```

        let (res, res_type) =
            parse_infer_and_eval(r#"map (let f = (λx → - (id x)) in f) [3.14, 6.9, 42.0, 1.0]"#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(float!()));
        assert_expr_eq!(res, l!(f!(-3.14), f!(-6.9), f!(-42.0), f!(-1.0)); ignore span);

        // FIXME(loong): this test is not passing.
        // ```rex
        // let (res, res_type) = parse_infer_and_eval(
        //     r#"map (let f = (λx → id (-(id x))) in f) [3.14, 6.9, 42.0, 1.0]"#,
        // )
        // .await
        // .unwrap();
        // assert_eq!(res_type, list!(float!()));
        // assert_expr_eq!(res, l!(f!(-3.14), f!(-6.9), f!(-42.0), f!(-1.0)); ignore span);
        // ```

        // FIXME(loong): this test is not passing.
        // ```rex
        // let (res, res_type) = parse_infer_and_eval(r#"map (id (λx → -x)) [3.14, 6.9, 42.0, 1.0]"#)
        //     .await
        //     .unwrap();
        // assert_eq!(res_type, list!(float!()));
        // assert_expr_eq!(res, l!(f!(-3.14), f!(-6.9), f!(-42.0), f!(-1.0)); ignore span);
        // ```
    }

    #[tokio::test]
    async fn test_compose() {
        let (res, res_type) = parse_infer_and_eval(r#"(((*) 6.9) . ((+) 42.0)) 3.14"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(311.466); ignore span);

        let (res, res_type) =
            parse_infer_and_eval(r#"let f = ((*) 6.9), g = ((+) 42.0) in (f . g) 3.14"#)
                .await
                .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(311.466); ignore span);

        let (res, res_type) = parse_infer_and_eval(r#"((λx → x * 6.9) . (λx → x + 42.0)) 3.14"#)
            .await
            .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(311.466); ignore span);

        let (res, res_type) =
            parse_infer_and_eval(r#"let f = (λx → x * 6.9), g = (λx → x + 42.0) in (f . g) 3.14"#)
                .await
                .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(311.466); ignore span);
    }

    #[tokio::test]
    async fn test_map_map() -> Result<(), String> {
        let (res, res_type) =
            parse_infer_and_eval(r#"let f = (λx → -x) in map f (map f [3.14, 6.9, 42.0, 1.0])"#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(float!()));
        assert_expr_eq!(res, l!(f!(3.14), f!(6.9), f!(42.0), f!(1.0)); ignore span);

        Ok(())
    }

    #[tokio::test]
    async fn test_map_extensive() -> Result<(), String> {
        let (res, res_type) =
            parse_infer_and_eval(r#"
                map
                    (let
                        g = λx → 2.0 * (id x) - x
                    in
                        g)
                    (let
                        f = λx → -(id x),
                        h = map f (map f [-1, -2, -3, -4])
                    in
                        map
                            f
                            (map
                                (λx → f (id x))
                                [3.28 - 0.14, id 6.9, (λx → x) 42.0, f (f 1.0)]))
                "#)
                .await
                .unwrap();
        assert_eq!(res_type, list!(float!()));
        assert_expr_eq!(res, l!(f!(3.1399999999999997), f!(6.9), f!(42.0), f!(1.0)); ignore span);
        Ok(())
    }

    #[tokio::test]
    async fn test_lambda_let_in_var() -> Result<(), String> {
        let (res, res_type) =
            parse_infer_and_eval(r#"(λx → let y = id x in y + y) 6.9"#)
                .await
                .unwrap();
        assert_eq!(res_type, float!());
        assert_expr_eq!(res, f!(13.8); ignore span);
        Ok(())
    }

    /// This test is meant to reflect the kind of usage pattern that we see in
    /// production code. However, it should not be taken as a comprehensive.
    #[tokio::test]
    async fn test_big_boy() {
        let (res, res_type) = parse_infer_and_eval(r#"
            (λxs ys zs →
                let
                    t = map (λx → ((get 0 xs) * x)) xs,
                    u = ys ++ [420],
                    v = take 2 zs,
                    f = (λx →
                        let
                            a = (id x)
                        in
                            a + a
                    ),
                    g = (++) ((++) xs t)
                in
                    zip (g xs) [[u], v]
            ) [2.0, 3.0, 4.0] [4, 5, 6] [[6, 7, 8], [9, 10, 11], [12, 13, 14]]
            "#)
        .await
        .unwrap();
        assert_eq!(res_type, list!(tuple!(float!(), list!(list!(uint!())))));
        assert_expr_eq!(
            res,
            l![
                tup!(f!(2.0), l![l![u!(4), u!(5), u!(6), u!(420)]]),
                tup!(f!(3.0), l![l![u!(6), u!(7), u!(8)], l![u!(9), u!(10), u!(11)]])
            ];
            ignore span
        );
    }
}
