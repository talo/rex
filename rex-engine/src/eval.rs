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
        Expr::Bool(..) | Expr::Uint(..) | Expr::Int(..) | Expr::Float(..) | Expr::String(..) => {
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
    println!("evaluated var: {}...", var);
    let pre_var_type = unify::apply_subst(ctx.env.read().await.get(&var.id).unwrap(), &ctx.subst);

    let var = match ctx.scope.get(&var.name) {
        None => var,
        Some(Expr::Var(var)) => return eval_var(ctx, var).await,
        Some(non_var_expr) => return eval(ctx, non_var_expr).await, // Ok(non_var_expr.clone()),
    };

    let var_type = unify::apply_subst(ctx.env.read().await.get(&var.id).unwrap(), &ctx.subst);

    let f = ctx.ftable.lookup_fns(&var.name, var_type.clone()).next();
    if let Some((f, f_type)) = f {
        if f_type.num_params() == 0 {
            let res = f(ctx, &vec![]).await?;
            ctx.env
                .write()
                .await
                .insert(*res.id(), pre_var_type.clone());
            return Ok(res);
        }
    }

    println!(" into: {}", var);
    Ok(Expr::Var(var.clone()))
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
    let pre_f_type = unify::apply_subst(
        ctx.env.read().await.get(f.borrow().id()).unwrap(),
        &ctx.subst,
    );
    let pre_x_type = unify::apply_subst(
        ctx.env.read().await.get(x.borrow().id()).unwrap(),
        &ctx.subst,
    );

    let (_pre_a_type, pre_b_type) = match &pre_f_type {
        Type::Arrow(a, b) => (a.clone(), b.clone()),
        _ => panic!("Function application on non-function type"),
    };

    let f = eval(ctx, f.borrow()).await?;
    let x = eval(ctx, x.borrow()).await?;

    let f_type = unify::apply_subst(
        ctx.env.read().await.get(f.borrow().id()).unwrap(),
        &ctx.subst,
    );
    let x_type = unify::apply_subst(
        ctx.env.read().await.get(x.borrow().id()).unwrap(),
        &ctx.subst,
    );

    let (_a_type, b_type) = match &pre_f_type {
        Type::Arrow(a, b) => (a.clone(), b.clone()),
        _ => panic!("Function application on non-function type"),
    };

    let res = match f {
        Expr::Var(var) => {
            // We need to reset the f_type because in order to do function
            // lookup, we need the f_type of the underlying callee, not the
            // result of applying the next argument to the callee (which, thanks
            // to curring, may result in another function).
            let f_type = pre_f_type; // unify::apply_subst(ctx.env.read().await.get(&var.id).unwrap(), &ctx.subst);

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
                        ctx.env.write().await.insert(*x.id(), pre_x_type.clone());

                        Expr::Curry(Id::new(), var.span, var, vec![x])
                    }
                }
            } else {
                panic!("Function not found: {}:{}", var.name, f_type)
            }
        }
        Expr::Lam(id, _span, scope, param, body) => {
            let l_type = unify::apply_subst(ctx.env.read().await.get(&id).unwrap(), &ctx.subst);

            let new_body_id = Id::new();
            let mut body = match *body {
                Expr::App(_, span, g, y) => {
                    let new_g_id = Id::new();
                    let new_y_id = Id::new();

                    let mut g = g.clone();
                    *g.id_mut() = new_g_id;
                    ctx.env.write().await.insert(new_g_id, pre_f_type.clone());

                    let mut y = y.clone();
                    *y.id_mut() = new_y_id;
                    ctx.env.write().await.insert(new_y_id, x_type.clone());

                    Expr::App(new_body_id, span.clone(), g, y)
                }
                body => body,
            };
            *body.id_mut() = new_body_id;
            ctx.env.write().await.insert(new_body_id, *pre_b_type);

            let mut ctx: Context<State> = ctx.clone();
            ctx.scope.insert_mut(param.name, x);
            for (k, v) in scope.iter() {
                ctx.scope.insert_mut(k.clone(), v.clone());
            }

            eval(&ctx, &body).await?
        }
        Expr::Curry(id, span, var, mut args) => {
            let f_type = unify::apply_subst(ctx.env.read().await.get(&var.id).unwrap(), &ctx.subst);

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
    let def = eval(ctx, def).await?;
    let mut ctx = ctx.clone();
    ctx.scope = ctx.scope.insert(var.name.clone(), def);
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
    use rex_ast::{assert_expr_eq, b, d, f, i, l, s, tup, u};
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

        // FIXME(loong): this test is not passing.
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

    // FIXME(loong): this test is not passing. This is caused by `num_params`
    // reporting all the parameters of the returned function. This is actually
    // what you want to do. But it means we need a better "edge case" when
    // calling curried expressions. Probably some kind of "call it in a loop
    // until all arguments are gone".
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
    async fn test_overloaded_let_polymorphism() {
        let (res, res_type) =
            parse_infer_and_eval(r#"let f = λx → -x in (f 6.9, f 420, f (int 314))"#)
                .await
                .unwrap();
        assert_eq!(res_type, tuple!(float!(), int!(), int!()));
        assert_expr_eq!(res, tup!(f!(-6.9), i!(-420), i!(-314)); ignore span);
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
    async fn test_let_in_cascade() {
        let (res, res_type) =
            parse_infer_and_eval(r#"let f = (λx → -x), u = f 6.9, v = f u in (u, v)"#)
                .await
                .unwrap();
        assert_eq!(res_type, tuple!(float!(), float!()));
        assert_expr_eq!(res, tup!(f!(-6.9), f!(6.9)); ignore span);
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
    async fn test_map_map() -> Result<(), String> {
        let mut parser = Parser::new(
            Token::tokenize("let f = (λx → -x) in map f (map f [3.14, 6.9, 42.0, 1.0])").unwrap(),
        );
        let expr = parser.parse_expr().unwrap();

        let builder = Builder::with_prelude().unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(&expr, &type_env, &mut expr_type_env, &mut constraint_system)
            .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, list![Type::Float]);

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
        match res {
            Ok(Expr::List(_, _, res)) => {
                assert!(res.len() == 4);
                assert!(matches!(res[0], Expr::Float(_, _, 3.14)));
                assert!(matches!(res[1], Expr::Float(_, _, 6.9)));
                assert!(matches!(res[2], Expr::Float(_, _, 42.0)));
                assert!(matches!(res[3], Expr::Float(_, _, 1.0)));
            }
            Err(e) => return Err(format!("{:?}", e)),
            _ => panic!("Expected [3.14, 6.9, 42.0, 1.0], got {:?}", res),
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_map_extensive() -> Result<(), String> {
        let mut parser = Parser::new(
            Token::tokenize(
                "map (let g = λx → 2.0 * (id x) - x in g) (let f = λx → -(id x), h = map f (map f [-1, -2, -3, -4]) in map f (map (λx → f (id x)) [3.28 - 0.14, id 6.9, (λx → x) 42.0, f (f 1.0)]))",
            )
            .unwrap(),
        );
        let expr = parser.parse_expr().unwrap();

        let builder = Builder::with_prelude().unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(&expr, &type_env, &mut expr_type_env, &mut constraint_system)
            .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;
        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "EXPR\n{}",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst))
        );

        assert_eq!(final_type, list![Type::Float]);

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
        match res {
            Ok(Expr::List(_, _, res)) => {
                assert!(res.len() == 4);
                assert!(matches!(res[0], Expr::Float(_, _, 3.1399999999999997)));
                assert!(matches!(res[1], Expr::Float(_, _, 6.9)));
                assert!(matches!(res[2], Expr::Float(_, _, 42.0)));
                assert!(matches!(res[3], Expr::Float(_, _, 1.0)));
            }
            Err(e) => return Err(format!("{:?}", e)),
            _ => panic!("Expected (6.9, 420, true), got {:?}", res),
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_lambda_let_in_var() -> Result<(), String> {
        let mut parser = Parser::new(Token::tokenize("(λx → let y = id x in y + y) 6.9").unwrap());
        let expr = parser.parse_expr().unwrap();

        let builder = Builder::with_prelude().unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(&expr, &type_env, &mut expr_type_env, &mut constraint_system)
            .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;
        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "EXPR\n{}\n",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst))
        );

        assert_eq!(final_type, Type::Float);

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

        match res {
            Ok(Expr::Float(_, _, 13.8)) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
            _ => panic!("Expected 13.8, got {:?}", res),
        }
    }

    /// This test is meant to reflect the kind of usage pattern that we see in
    /// production code. However, it should not be taken as a comprehensive.
    #[tokio::test]
    async fn test_big_boy() {
        let (res, res_type) = parse_infer_and_eval(
            r#"
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
"#,
        )
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
