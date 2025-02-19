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

    // println!(
    //     "pre-eval apply function: ({{{}}}:{}) ({}:{})",
    //     f.borrow(),
    //     &pre_f_type,
    //     x.borrow(),
    //     &pre_x_type
    // );

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

    // println!(
    //     "post-eval apply function: ({{{}}}:{}) ({}:{})",
    //     &f, &f_type, &x, &x_type
    // );

    let res = match f {
        Expr::Var(var) => {
            // We need to reset the f_type because in order to do function
            // lookup, we need the f_type of the underlying callee, not the
            // result of applying the next argument to the callee (which, thanks
            // to curring, may result in another function).
            let f_type = pre_f_type; // unify::apply_subst(ctx.env.read().await.get(&var.id).unwrap(), &ctx.subst);

            println!(
                "var function lookup: ({}:{}) ({}:{})",
                &var, &f_type, &x, &x_type
            );

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
                    // TODO(loong): fix the ID and the span.
                    _ => {
                        println!(
                            "curry function lookup: ({}:{}) ({}:{})",
                            &var, &f_type, &x, &x_type
                        );

                        let mut x = x.clone();
                        *x.id_mut() = Id(rand::random());
                        ctx.env.write().await.insert(*x.id(), pre_x_type.clone());

                        Expr::Curry(Id(rand::random()), var.span, var, vec![x])
                    }
                }
            } else {
                panic!("Function not found: {}:{}", var.name, f_type)
            }
        }
        Expr::Lam(id, _span, scope, param, body) => {
            let l_type = unify::apply_subst(ctx.env.read().await.get(&id).unwrap(), &ctx.subst);

            // println!(
            //     "dropping into lambda: ({}) [actually: {}] with param: ({}:{})",
            //     &l_type, &pre_f_type, &x, &x_type
            // );

            // println!("hot-swap lambda body: {{{}}}:{}", &body, &pre_b_type);

            let new_body_id = Id(rand::random());
            let mut body = match *body {
                Expr::App(_, span, g, y) => {
                    let new_g_id = Id(rand::random());
                    let new_y_id = Id(rand::random());

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

                if let Some((f, _ftype)) = f {
                    println!(
                        "calling curried function: ({}:{}) ({}:{}) results in type: {}",
                        &var, &f_type, &x, &x_type, &b_type
                    );
                    f(ctx, &args).await?
                } else {
                    panic!("Function not found: {}:{}", var.name, f_type)
                }
            } else {
                // TODO(loong): fix the ID and the span.
                Expr::Curry(Id(rand::random()), span, var, args)
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
    use rex_ast::id::IdDispenser;
    use rex_lexer::Token;
    use rex_parser::Parser;
    use rex_type_system::{
        constraint::generate_constraints,
        list,
        trace::{sprint_expr_with_type, sprint_subst, sprint_type_env},
        tuple,
        types::Type,
        unify::{self},
    };

    use crate::engine::Builder;

    use super::*;

    #[tokio::test]
    async fn test_simple() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("1").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system).unwrap();

        let final_type = unify::apply_subst(&ty, &subst);
        assert!(final_type == Type::Uint);

        let res = eval(
            &Context {
                scope: Scope::new_sync(),
                ftable,
                subst,
                env: Arc::new(RwLock::new(expr_type_env)), // NOTE(loong): stop talking shit about me.
                state: (),
            },
            &expr,
        )
        .await
        .unwrap();
        assert!(matches!(res, Expr::Uint(_, _, 1)));
    }

    #[tokio::test]
    async fn test_negate() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("-3.14").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        dbg!(&constraint_system);
        dbg!(&type_env);

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system).unwrap();

        let final_type = unify::apply_subst(&ty, &subst);
        assert!(final_type == Type::Float);

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
        .await
        .unwrap();
        assert!(matches!(res, Expr::Float(_, _, -3.14)));
    }

    #[tokio::test]
    async fn test_negate_tuple() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(-6.9, -420)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system).unwrap();

        let final_type = unify::apply_subst(&ty, &subst);
        assert!(final_type == Type::Tuple(vec![Type::Float, Type::Int]));

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
        .await
        .unwrap();
        match res {
            Expr::Tuple(_, _, res) => {
                assert!(res.len() == 2);
                assert!(matches!(res[0], Expr::Float(_, _, -6.9)));
                assert!(matches!(res[1], Expr::Int(_, _, -420)));
            }
            _ => panic!("Expected (-6.9, -420), got {:?}", res),
        }
    }

    #[tokio::test]
    async fn test_negate_list() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("[-6.9, -3.14]").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system).unwrap();

        let final_type = unify::apply_subst(&ty, &subst);
        assert!(final_type == Type::List(Type::Float.into()));

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
        .await
        .unwrap();
        match res {
            Expr::List(_, _, res) => {
                assert!(res.len() == 2);
                assert!(matches!(res[0], Expr::Float(_, _, -6.9)));
                assert!(matches!(res[1], Expr::Float(_, _, -3.14)));
            }
            _ => panic!("Expected [-6.9, -420], got {:?}", res),
        }
    }

    #[tokio::test]
    async fn test_add() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("6.9 + 4.20").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system).unwrap();

        let final_type = unify::apply_subst(&ty, &subst);
        assert!(final_type == Type::Float);

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
        .await
        .unwrap();
        assert!(matches!(res, Expr::Float(_, _, 11.100000000000001)));
    }

    #[tokio::test]
    async fn test_add_then_add() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser =
            Parser::new(Token::tokenize("let f = (\\x -> (x+x)) in (f (6.9 + 3.14))").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::<()>::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system).unwrap();

        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Float);

        println!(
            "\n{}\n",
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
        .await
        .unwrap();
        assert!(matches!(res, Expr::Float(_, _, 20.080000000000002)));
    }

    #[tokio::test]
    async fn test_id_then_add() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("let f = id in (f 6.9) + (f 3.14)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::<()>::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system).unwrap();

        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Float);

        println!(
            "\n{}\n",
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
        .await
        .unwrap();
        assert!(matches!(dbg!(res), Expr::Float(_, _, 10.040000000000001)));
    }

    #[tokio::test]
    async fn test_add_tuple() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(6.9 + 4.20, 6 + 9)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system).unwrap();

        let final_type = unify::apply_subst(&ty, &subst);
        assert!(final_type == Type::Tuple(vec![Type::Float, Type::Uint]));

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
        .await
        .unwrap();
        match res {
            Expr::Tuple(_, _, res) => {
                assert!(res.len() == 2);
                assert!(matches!(res[0], Expr::Float(_, _, 11.100000000000001)));
                assert!(matches!(res[1], Expr::Uint(_, _, 15)));
            }
            _ => panic!("Expected (11.100000000000001, 15), got {:?}", res),
        }
    }

    #[tokio::test]
    async fn test_polymorphism() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(id 6.9, id 420)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;

        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Tuple(vec![Type::Float, Type::Uint]));

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
        .await
        .unwrap();
        match res {
            Expr::Tuple(_, _, res) => {
                assert!(res.len() == 2);
                assert!(matches!(res[0], Expr::Float(_, _, 6.9)));
                assert!(matches!(res[1], Expr::Uint(_, _, 420)));
            }
            _ => panic!("Expected (6.9, 420), got {:?}", res),
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_let_polymorphism() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser =
            Parser::new(Token::tokenize("let jd = λx → x in (jd 6.9, jd 420)").unwrap()); // Yes, we do mean jd
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;

        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, Type::Tuple(vec![Type::Float, Type::Uint]));

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
        .await
        .unwrap();

        match res {
            Expr::Tuple(_, _, res) => {
                assert!(res.len() == 2);
                assert!(matches!(res[0], Expr::Float(_, _, 6.9)));
                assert!(matches!(res[1], Expr::Uint(_, _, 420)));
            }
            _ => panic!("Expected (6.9, 420), got {:?}", res),
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_let_polymorphism_overloading() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser =
            Parser::new(Token::tokenize("let f = λx → -x in (f 6.9, f 420, f (3 + 14))").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();
        let mut expr_type_env = ExprTypeEnv::new();

        // println!("BEFORE\n",);
        // println!("TYPE_ENV\n{}", sprint_type_env(&type_env));
        // println!("CONSTRAINTS\n{}", constraint_system);

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;

        println!(
            "AFTER\n{}",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst))
        );
        println!("SUBST\n{}", sprint_subst(&subst));
        println!("TYPE_ENV\n{}", sprint_type_env(&type_env));
        println!("CONSTRAINTS\n{}", constraint_system);

        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(
            final_type,
            Type::Tuple(vec![Type::Float, Type::Int, Type::Int])
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
        match res {
            Ok(Expr::Tuple(_, _, res)) => {
                assert!(res.len() == 3);
                assert!(matches!(res[0], Expr::Float(_, _, -6.9)));
                assert!(matches!(res[1], Expr::Int(_, _, -420)));
                assert!(matches!(res[2], Expr::Int(_, _, -17)));
            }
            Err(e) => return Err(format!("{:?}", e)),
            _ => panic!("Expected (-6.9, -420, 17), got {:?}", res),
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_parametric_ftable() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser =
            Parser::new(Token::tokenize("let f = λx → id x in (f 6.9, f 420, f true)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;

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
            Ok(Expr::Tuple(_, _, res)) => {
                assert!(res.len() == 3);
                assert!(matches!(res[0], Expr::Float(_, _, 6.9)));
                assert!(matches!(res[1], Expr::Uint(_, _, 420)));
                assert!(matches!(res[2], Expr::Bool(_, _, true)));
            }
            Err(e) => return Err(format!("{:?}", e)),
            _ => panic!("Expected (6.9, 420, true), got {:?}", res),
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_let_id() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("let f = id in f 6.9").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;
        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "EXPR\n{}",
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
            Ok(Expr::Float(_, _, 6.9)) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
            _ => panic!("Expected 6.9, got {:?}", res),
        }
    }

    #[tokio::test]
    async fn test_zero() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser =
            Parser::new(Token::tokenize("(6.9 + zero, 420 + zero, -314 + zero)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;
        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "EXPR\n{}",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst))
        );

        assert_eq!(final_type, tuple!(Type::Float, Type::Uint, Type::Int));

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
        .await
        .unwrap();

        match res {
            Expr::Tuple(_, _, res) => {
                assert!(res.len() == 3);
                assert!(matches!(res[0], Expr::Float(_, _, 6.9)));
                assert!(matches!(res[1], Expr::Uint(_, _, 420)));
                assert!(matches!(res[2], Expr::Int(_, _, -314)));
                Ok(())
            }
            _ => panic!("Expected (-6.9, 420), got {:?}", res),
        }
    }

    #[tokio::test]
    async fn test_let_negate() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("let f = negate in f 6.9").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;
        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "EXPR\n{}",
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
            Ok(Expr::Float(_, _, -6.9)) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
            _ => panic!("Expected (-6.9, 420), got {:?}", res),
        }
    }

    #[tokio::test]
    async fn test_let_in_let_in() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(
            Token::tokenize("let f = (λx → -x), u = f 6.9, v = f 420 in (u, v)").unwrap(),
        );
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;
        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "EXPR\n{}",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst))
        );

        assert_eq!(final_type, tuple!(Type::Float, Type::Int));

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
        .await
        .unwrap();

        match res {
            Expr::Tuple(_, _, res) => {
                assert!(res.len() == 2);
                assert!(matches!(res[0], Expr::Float(_, _, -6.9)));
                assert!(matches!(res[1], Expr::Int(_, _, -420)));
                Ok(())
            }
            _ => panic!("Expected (-6.9, 420), got {:?}", res),
        }
    }

    #[tokio::test]
    async fn test_map() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(
            Token::tokenize("let f = (λx → -x) in map f [3.14, 6.9, 42.0, 1.0]").unwrap(),
        );
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;
        let final_type = unify::apply_subst(&ty, &subst);
        assert_eq!(final_type, list![Type::Float]);

        println!(
            "EXPR\n{}",
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
        match res {
            Ok(Expr::List(_, _, res)) => {
                assert!(res.len() == 4);
                assert!(matches!(res[0], Expr::Float(_, _, -3.14)));
                assert!(matches!(res[1], Expr::Float(_, _, -6.9)));
                assert!(matches!(res[2], Expr::Float(_, _, -42.0)));
                assert!(matches!(res[3], Expr::Float(_, _, -1.0)));
            }
            Err(e) => return Err(format!("{:?}", e)),
            _ => panic!("Expected [-3.14, -6.9, -42.0, -1.0], got {:?}", res),
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_map_map() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(
            Token::tokenize("let f = (λx → -x) in map f (map f [3.14, 6.9, 42.0, 1.0])").unwrap(),
        );
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
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
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(
            Token::tokenize(
                "map (let g = λx → 2.0 * (id x) - x in g) (let f = λx → -(id x), h = map f (map f [-1, -2, -3, -4]) in map f (map (λx → f (id x)) [3.28 - 0.14, id 6.9, (λx → x) 42.0, f (f 1.0)]))",
            )
            .unwrap(),
        );
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
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
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(λx → let y = id x in y + y) 6.9").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
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

    #[tokio::test]
    async fn test_realistic() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(
            Token::tokenize(
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
) 
    [2.0, 3.0, 4.0]
    [4, 5, 6] 
    [[6, 7, 8], [9, 10, 11], [12, 13, 14]]
"#,
            )
            .unwrap(),
        );
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let builder = Builder::with_prelude(&mut id_dispenser).unwrap();
        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let subst = unify::unify_constraints(&constraint_system)?;
        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "EXPR\n{}\n",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst))
        );
        println!("FINAL TYPE\n{}", final_type);

        assert_eq!(
            final_type,
            list![tuple!(Type::Float, list!(list!(Type::Uint)))]
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
        .await
        .unwrap();

        println!("res: {}", res);
        match res {
            Expr::List(_, _, res) => {
                assert!(res.len() == 2);
                match &res[0] {
                    Expr::Tuple(_, _, res) => {
                        assert!(res.len() == 2);
                        assert!(matches!(&res[0], Expr::Float(_, _, 2.0)));
                        match &res[1] {
                            Expr::List(_, _, res) => {
                                assert!(res.len() == 1);
                                match &res[0] {
                                    Expr::List(_, _, res) => {
                                        assert!(res.len() == 4);
                                        assert!(matches!(res[0], Expr::Uint(_, _, 4)));
                                        assert!(matches!(res[1], Expr::Uint(_, _, 5)));
                                        assert!(matches!(res[2], Expr::Uint(_, _, 6)));
                                        assert!(matches!(res[3], Expr::Uint(_, _, 420)));
                                    }
                                    _ => panic!("Expected [4, 5, 6, 420], got {:?}", res),
                                }
                            }
                            _ => panic!("Expected [[4, 5, 6, 420]], got {:?}", res),
                        }
                    }
                    _ => panic!("Expected (2, [[4, 5, 6, 420]]), got {:?}", res),
                }
                match &res[1] {
                    Expr::Tuple(_, _, res) => {
                        assert!(res.len() == 2);
                        assert!(matches!(&res[0], Expr::Float(_, _, 3.0)));
                        match &res[1] {
                            Expr::List(_, _, res) => {
                                assert!(res.len() == 2);
                                match &res[0] {
                                    Expr::List(_, _, res) => {
                                        assert!(res.len() == 3);
                                        assert!(matches!(res[0], Expr::Uint(_, _, 6)));
                                        assert!(matches!(res[1], Expr::Uint(_, _, 7)));
                                        assert!(matches!(res[2], Expr::Uint(_, _, 8)));
                                    }
                                    _ => panic!("Expected [6, 7, 8], got {:?}", res),
                                }
                                match &res[1] {
                                    Expr::List(_, _, res) => {
                                        assert!(res.len() == 3);
                                        assert!(matches!(res[0], Expr::Uint(_, _, 9)));
                                        assert!(matches!(res[1], Expr::Uint(_, _, 10)));
                                        assert!(matches!(res[2], Expr::Uint(_, _, 11)));
                                    }
                                    _ => panic!("Expected [9, 10, 11], got {:?}", res),
                                }
                            }
                            _ => panic!("Expected [[6, 7, 8], [9, 10, 11]], got {:?}", res),
                        }
                    }
                    _ => panic!("Expected (3.0, [6, 7, 8], [9, 10, 11]]), got {:?}", res),
                }
            }
            _ => panic!(
                "Expected [(2.0, [[4, 5, 6, 420]]), (3.0, [6, 7, 8], [9, 10, 11]])], got {:?}",
                res
            ),
        }

        Ok(())
    }
}
