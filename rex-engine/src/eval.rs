use std::{borrow::Borrow, collections::BTreeMap, sync::Arc};

use futures::future;
use rex_ast::{
    expr::{Expr, Var},
    id::Id,
};
use rex_lexer::span::Span;
use rex_type_system::{
    types::{ExprTypeEnv, Type},
    unify::{self, Subst},
};
use rpds::HashTrieMapSync;
use tokio::sync::RwLock;

use crate::{error::Error, ftable::Ftable};

pub type Scope = HashTrieMapSync<String, Expr>;

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
        Expr::Lam(id, span, param, body) => eval_lam(ctx, id, span, param, body).await,
        Expr::Let(id, span, var, def, body) => eval_let(ctx, id, span, var, def, body).await,
        Expr::Ite(id, span, cond, then, r#else) => {
            eval_ite(ctx, id, span, cond, then, r#else).await
        }
        Expr::Curry(_id, _span, _f, _args) => {
            todo!("eval the curry by checking if it has enough args")
        }
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

pub async fn eval_var<State>(ctx: &Context<State>, var: &Var) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    let var = match ctx.scope.get(&var.name) {
        None => var,
        Some(Expr::Var(var)) => var,
        Some(non_var_expr) => return Ok(non_var_expr.clone()),
    };

    let var_type = unify::apply_subst(ctx.env.read().await.get(&var.id).unwrap(), &ctx.subst);
    let f = ctx.ftable.lookup_fns(&var.name, var_type.clone()).next();
    if let Some((f, _ftype)) = f {
        if var_type.num_params() == 0 {
            return f(ctx, &vec![]).await;
        }
    }

    Ok(Expr::Var(var.clone()))
}

pub async fn eval_app<State>(
    ctx: &Context<State>,
    _id: &Id,
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
    let f = eval(ctx, f.borrow()).await?;
    let x = eval(ctx, x.borrow()).await?;

    match f {
        Expr::Var(var) => {
            let var_type =
                unify::apply_subst(ctx.env.read().await.get(&var.id).unwrap(), &ctx.subst);

            let x_type = unify::apply_subst(ctx.env.read().await.get(&x.id()).unwrap(), &ctx.subst);

            let var_type = match var_type {
                Type::Arrow(a, b) => Type::Arrow(Box::new(x_type.clone()), b),
                _ => panic!("Function application on non-function type"),
            };

            println!("XTYPE {}", x_type);

            // TODO(loong): we should be checking if more than one function is
            // found. This is an ambiguity error.
            let f = ctx.ftable.lookup_fns(&var.name, var_type.clone()).next();
            if let Some((f, ftype)) = f {
                match var_type.num_params() {
                    0 => panic!("Function application on non-function type"),
                    1 => {
                        match f(ctx, &vec![x]).await {
                            Ok(expr) => {
                                match ftype {
                                    // FIXME(loong): this genius block of code
                                    // needs to be applied everywhere that
                                    // ftable functions are invoked.
                                    Type::Arrow(_a, b) => {
                                        let mut env = ctx.env.write().await;

                                        // FIXME(loong): the return type of
                                        // ftype could be a type variable. And
                                        // we need to resolve that to an actual
                                        // type. This might be a problem.
                                        env.insert(*expr.id(), *b.clone());
                                        Ok(expr)
                                    }
                                    _ => unreachable!(), // NOTE(loong): assuming type inference is working, and has been done, it should never be possible to apply a non-arrow type.
                                }
                            }
                            Err(e) => Err(e),
                        }
                    }
                    _ => Ok(Expr::Curry(var.id, var.span, var, vec![x])), // TODO(loong): fix the ID and the span.
                }
            } else {
                panic!("Function not found: {}:{}", var.name, var_type)
            }
        }
        Expr::Lam(_id, _span, param, body) => {
            let mut ctx = ctx.clone();
            ctx.scope = ctx.scope.insert(param.name, x);
            eval(&ctx, &body).await
        }
        Expr::Curry(id, span, var, mut args) => {
            args.push(x);
            let var_type =
                unify::apply_subst(ctx.env.read().await.get(&var.id).unwrap(), &ctx.subst);

            // TODO(loong): we should be checking if more than one function is
            // found. This is an ambiguity error.
            let f = ctx.ftable.lookup_fns(&var.name, var_type.clone()).next();

            if let Some((f, _ftype)) = f {
                if var_type.num_params() < args.len() {
                    panic!("Too many arguments");
                } else if var_type.num_params() == args.len() {
                    match f(&ctx, &args).await {
                        Ok(expr) => Ok(expr),
                        Err(e) => Err(e),
                    }
                } else {
                    Ok(Expr::Curry(id, span, var, args)) // TODO(loong): fix the ID and the span.
                }
            } else {
                panic!("Function not found: {}", var.name)
            }
        }
        _ => unimplemented!(),
    }
}

pub async fn eval_lam<State>(
    _ctx: &Context<State>,
    id: &Id,
    span: &Span,
    param: &Var,
    body: &Expr,
) -> Result<Expr, Error>
where
    State: Clone + Send + Sync + 'static,
{
    Ok(Expr::Lam(
        id.clone(),
        span.clone(),
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
        trace::{sprint_expr_with_type, sprint_subst, sprint_type_env},
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
            Parser::new(Token::tokenize("let f = λx → -x in (f 6.9, f 420, f (-1))").unwrap());
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

        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "AFTER\n{}",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst))
        );
        println!("SUBST\n{}", sprint_subst(&subst));
        println!("TYPE_ENV\n{}", sprint_type_env(&type_env));
        println!("CONSTRAINTS\n{}", constraint_system);

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
                assert!(matches!(res[2], Expr::Int(_, _, 1)));
            }
            Err(e) => return Err(format!("{:?}", e)),
            _ => panic!("Expected (-6.9, -420, 1), got {:?}", res),
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
}
