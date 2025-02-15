use std::collections::BTreeMap;

use futures::future;
use rex_ast::{
    expr::{Expr, Var},
    id::Id,
};
use rex_lexer::span::Span;
use rex_type_system::{
    types::ExprTypeEnv,
    unify::{self, Subst},
};
use rpds::HashTrieMapSync;

use crate::{error::Error, ftable::Ftable};

pub type Scope = HashTrieMapSync<String, Expr>;

#[async_recursion::async_recursion]
pub async fn eval(ctx: &Context, expr: Expr) -> Result<Expr, Error> {
    match expr {
        Expr::Bool(..) | Expr::Uint(..) | Expr::Int(..) | Expr::Float(..) | Expr::String(..) => {
            Ok(expr)
        }
        Expr::Tuple(id, span, tuple) => eval_tuple(ctx, id, span, tuple).await,
        Expr::List(id, span, list) => eval_list(ctx, id, span, list).await,
        Expr::Dict(id, span, dict) => eval_dict(ctx, id, span, dict).await,
        Expr::Var(var) => eval_var(ctx, var).await,
        Expr::App(id, span, f, x) => eval_app(ctx, id, span, *f, *x).await,
        Expr::Lam(id, span, param, body) => eval_lam(ctx, id, span, param, *body).await,
        Expr::Let(id, span, var, def, body) => eval_let(ctx, id, span, var, *def, *body).await,
        Expr::Ite(id, span, cond, then, r#else) => {
            eval_ite(ctx, id, span, *cond, *then, *r#else).await
        }
        Expr::Curry(id, span, f, args) => todo!("eval the curry by checking if it has enough args"),
    }
}

pub async fn eval_tuple(
    ctx: &Context,
    id: Id,
    span: Span,
    tuple: Vec<Expr>,
) -> Result<Expr, Error> {
    let mut result = Vec::with_capacity(tuple.len());
    for v in tuple {
        result.push(eval(ctx, v));
    }
    Ok(Expr::Tuple(
        id,
        span,
        future::join_all(result)
            .await
            .into_iter()
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

pub async fn eval_list(ctx: &Context, id: Id, span: Span, list: Vec<Expr>) -> Result<Expr, Error> {
    let mut result = Vec::with_capacity(list.len());
    for v in list {
        result.push(eval(ctx, v));
    }
    Ok(Expr::List(
        id,
        span,
        future::join_all(result)
            .await
            .into_iter()
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

pub async fn eval_dict(
    ctx: &Context,
    id: Id,
    span: Span,
    dict: BTreeMap<String, Expr>,
) -> Result<Expr, Error> {
    let mut keys = Vec::with_capacity(dict.len());
    let mut vals = Vec::with_capacity(dict.len());
    for (k, v) in dict {
        keys.push(k);
        vals.push(eval(ctx, v));
    }

    let mut result = BTreeMap::new();
    for (k, v) in keys.into_iter().zip(future::join_all(vals).await) {
        result.insert(k, v?);
    }
    Ok(Expr::Dict(id, span, result))
}

pub async fn eval_var(ctx: &Context, var: Var) -> Result<Expr, Error> {
    let val = match ctx.scope.get(&var.name) {
        Some(val) => val,
        None => &Expr::Var(var),
    };

    match val {
        Expr::Var(var) => {
            let var_type = ctx.env.get(&var.id).unwrap();
            let var_type = unify::apply_subst(var_type, &ctx.subst);
            let f = ctx.ftable.lookup_fns(&var.name, var_type.clone()).next();
            if let Some(f) = f {
                if var_type.num_params() == 0 {
                    return f(ctx, &vec![]).await;
                }
            }
            Ok(Expr::Var(var.clone()))
        }
        _ => Ok(val.clone()),
    }
}

pub async fn eval_app(ctx: &Context, id: Id, span: Span, f: Expr, x: Expr) -> Result<Expr, Error> {
    let f = eval(ctx, f).await?;
    let x = eval(ctx, x).await?;

    match f {
        Expr::Var(var) => {
            let var_type = ctx.env.get(&var.id).unwrap();
            let var_type = unify::apply_subst(var_type, &ctx.subst);
            let f = ctx.ftable.lookup_fns(&var.name, var_type.clone()).next();
            if let Some(f) = f {
                match var_type.num_params() {
                    0 => panic!("Function application on non-function type"),
                    1 => f(ctx, &vec![x]).await,
                    _ => Ok(Expr::Curry(var.id, var.span, var, vec![x])), // TODO(loong): fix the ID and the span.
                }
            } else {
                panic!("Function not found: {}:{}", var.name, var_type)
            }
        }
        Expr::Lam(_id, _span, param, body) => {
            let mut ctx = ctx.clone();
            ctx.scope = ctx.scope.insert(param.name, x);
            eval(&ctx, *body).await
        }
        Expr::Curry(id, span, var, mut args) => {
            args.push(x);
            let var_type = ctx.env.get(&var.id).unwrap();
            let var_type = unify::apply_subst(var_type, &ctx.subst);
            let f = ctx.ftable.lookup_fns(&var.name, var_type.clone()).next();
            if let Some(f) = f {
                if var_type.num_params() < args.len() {
                    panic!("Too many arguments");
                } else if var_type.num_params() == args.len() {
                    f(&ctx, &args).await
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

#[derive(Clone)]
pub struct Context {
    scope: Scope,
    ftable: Ftable,
    subst: Subst,
    env: ExprTypeEnv,
}

pub async fn eval_lam(
    ctx: &Context,
    id: Id,
    span: Span,
    param: Var,
    body: Expr,
) -> Result<Expr, Error> {
    Ok(Expr::Lam(id, span, param, Box::new(body)))
}

pub async fn eval_let(
    ctx: &Context,
    id: Id,
    span: Span,
    var: Var,
    def: Expr,
    body: Expr,
) -> Result<Expr, Error> {
    let def = eval(ctx, def).await?;
    let mut ctx = ctx.clone();
    ctx.scope = ctx.scope.insert(var.name, def);
    eval(&ctx, body).await
}

pub async fn eval_ite(
    ctx: &Context,
    id: Id,
    span: Span,
    cond: Expr,
    then: Expr,
    r#else: Expr,
) -> Result<Expr, Error> {
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
        arrow,
        constraint::{generate_constraints, Constraint, ConstraintSystem},
        types::{Type, TypeEnv},
        unify::{self},
    };

    use crate::engine::Builder;

    use super::*;

    #[tokio::test]
    async fn test_simple() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("1").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let ftable = Ftable::with_prelude();
        let type_env = TypeEnv::new();

        let mut constraint_system = ConstraintSystem::new();
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
                env: expr_type_env,
            },
            expr,
        )
        .await
        .unwrap();
        assert!(matches!(res, Expr::Uint(_, _, 1)));
    }

    #[tokio::test]
    async fn test_negate() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("negate 3.14").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let mut builder = Builder::new();

        let ftable = Ftable::with_prelude();

        let mut type_env = TypeEnv::new();
        let negate_id = id_dispenser.next();
        type_env.insert("negate".to_string(), Type::Var(negate_id));

        let mut constraint_system = ConstraintSystem::new();
        constraint_system.add_global_constraint(Constraint::OneOf(
            Type::Var(negate_id),
            vec![
                arrow!(Type::Uint =>  Type::Int),
                arrow!(Type::Int =>  Type::Int),
                arrow!(Type::Float => Type::Float),
            ]
            .into_iter()
            .collect(),
        ));

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
                env: expr_type_env,
            },
            expr,
        )
        .await
        .unwrap();
        assert!(matches!(res, Expr::Float(_, _, -3.14)));
    }

    #[tokio::test]
    async fn test_negate_tuple() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("(negate 6.9, negate 420)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let ftable = Ftable::with_prelude();

        let mut type_env = TypeEnv::new();
        let negate_id = id_dispenser.next();
        type_env.insert("negate".to_string(), Type::Var(negate_id));

        let mut constraint_system = ConstraintSystem::new();
        constraint_system.add_global_constraint(Constraint::OneOf(
            Type::Var(negate_id),
            vec![
                arrow!(Type::Uint =>  Type::Int),
                arrow!(Type::Int =>  Type::Int),
                arrow!(Type::Float => Type::Float),
            ]
            .into_iter()
            .collect(),
        ));

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
                env: expr_type_env,
            },
            expr,
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
        let mut parser = Parser::new(Token::tokenize("[negate 6.9, negate 3.14]").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let ftable = Ftable::with_prelude();

        let mut type_env = TypeEnv::new();
        let negate_id = id_dispenser.next();
        type_env.insert("negate".to_string(), Type::Var(negate_id));

        let mut constraint_system = ConstraintSystem::new();
        constraint_system.add_global_constraint(Constraint::OneOf(
            Type::Var(negate_id),
            vec![
                arrow!(Type::Uint =>  Type::Int),
                arrow!(Type::Int =>  Type::Int),
                arrow!(Type::Float => Type::Float),
            ]
            .into_iter()
            .collect(),
        ));

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
                env: expr_type_env,
            },
            expr,
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

        let ftable = Ftable::with_prelude();

        let mut type_env = TypeEnv::new();
        let add_id = id_dispenser.next();
        type_env.insert("+".to_string(), Type::Var(add_id));

        let mut constraint_system = ConstraintSystem::new();
        constraint_system.add_global_constraint(Constraint::OneOf(
            Type::Var(add_id),
            vec![
                arrow!(Type::Uint => arrow!(Type::Uint => Type::Uint)),
                arrow!(Type::Int => arrow!(Type::Int => Type::Int)),
                arrow!(Type::Float => arrow!(Type::Float => Type::Float)),
            ]
            .into_iter()
            .collect(),
        ));

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
                env: expr_type_env,
            },
            expr,
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

        let ftable = Ftable::with_prelude();

        let mut type_env = TypeEnv::new();
        let add_id = id_dispenser.next();
        type_env.insert("+".to_string(), Type::Var(add_id));

        let mut constraint_system = ConstraintSystem::new();
        constraint_system.add_global_constraint(Constraint::OneOf(
            Type::Var(add_id),
            vec![
                arrow!(Type::Uint => arrow!(Type::Uint => Type::Uint)),
                arrow!(Type::Int => arrow!(Type::Int => Type::Int)),
                arrow!(Type::Float => arrow!(Type::Float => Type::Float)),
            ]
            .into_iter()
            .collect(),
        ));

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
                env: expr_type_env,
            },
            expr,
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
        let state = ();

        let ftable = Ftable::with_prelude();
        let mut type_env = TypeEnv::new();

        let id_param_typeid = id_dispenser.next();
        type_env.insert(
            "id".to_string(),
            Type::ForAll(
                id_param_typeid.into(),
                arrow!(Type::Var(id_param_typeid) =>  Type::Var(id_param_typeid)).into(),
                Default::default(),
            ),
        );

        let mut constraint_system = ConstraintSystem::with_global_constraints(vec![]);
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

        let res = eval(
            &Context {
                scope: Scope::new_sync(),
                ftable,
                subst,
                env: expr_type_env,
            },
            expr,
        )
        .await
        .unwrap();

        Ok(())
    }

    #[tokio::test]
    async fn test_let_polymorphism() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser =
            Parser::new(Token::tokenize("let id = \\x -> x in (id 6.9, id 420)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        let state = ();

        let ftable = Ftable::with_prelude();
        let mut type_env = TypeEnv::new();

        let neg_op_typeid = id_dispenser.next();
        type_env.insert("negate".to_string(), Type::Var(neg_op_typeid));

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
                Type::Var(neg_op_typeid),
                vec![
                    arrow!(Type::Uint =>  Type::Int),
                    arrow!(Type::Int =>  Type::Int),
                    arrow!(Type::Float => Type::Float),
                ]
                .into_iter()
                .collect(),
            )]);
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

        let res = eval(
            &Context {
                scope: Scope::new_sync(),
                ftable,
                subst,
                env: expr_type_env,
            },
            expr,
        )
        .await
        .unwrap();

        Ok(())
    }

    #[tokio::test]
    async fn test_let_polymorphism_overloading() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser =
            Parser::new(Token::tokenize("let f = \\x -> negate x in (f 6.9, f 420)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let ftable = Ftable::with_prelude();
        let mut type_env = TypeEnv::new();

        let neg_op_typeid = id_dispenser.next();
        type_env.insert("negate".to_string(), Type::Var(neg_op_typeid));

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
                Type::Var(neg_op_typeid),
                vec![
                    arrow!(Type::Uint =>  Type::Int),
                    arrow!(Type::Int =>  Type::Int),
                    arrow!(Type::Float => Type::Float),
                ]
                .into_iter()
                .collect(),
            )]);
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
                env: expr_type_env,
            },
            expr,
        )
        .await;

        Ok(())
    }

    #[tokio::test]
    async fn test_parametric_ftable() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser =
            Parser::new(Token::tokenize("let f = \\x -> id x in (f 6.9, f 420, f true)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let ftable = Ftable::with_prelude();

        let mut type_env = TypeEnv::new();

        let id_param_typeid = id_dispenser.next();
        type_env.insert(
            "id".to_string(),
            Type::ForAll(
                id_param_typeid.into(),
                arrow!(Type::Var(id_param_typeid) =>  Type::Var(id_param_typeid)).into(),
                Default::default(),
            ),
        );

        let mut constraint_system = ConstraintSystem::with_global_constraints(vec![]);
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
                env: expr_type_env,
            },
            expr,
        )
        .await;

        Ok(())
    }
}
