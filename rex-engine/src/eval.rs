use std::{
    collections::{BTreeMap, HashMap},
    fmt::{self, Display, Formatter},
};

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

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Expr(Expr),
    Closure(Var, Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Expr(e) => e.fmt(f),
            Value::Closure(var, args) => {
                write!(f, "Closure({} {})", var.name, args.len())
            }
        }
    }
}

pub type Scope = HashTrieMapSync<String, Value>;

#[async_recursion::async_recursion]
pub async fn eval(
    scope: &Scope,
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    expr: Expr,
) -> Result<Value, Error> {
    match expr {
        Expr::Bool(..) => Ok(Value::Expr(expr)),
        Expr::Uint(..) => Ok(Value::Expr(expr)),
        Expr::Int(..) => Ok(Value::Expr(expr)),
        Expr::Float(..) => Ok(Value::Expr(expr)),
        Expr::String(..) => Ok(Value::Expr(expr)),
        Expr::Tuple(id, span, tuple) => {
            eval_tuple(scope, ftable, env, subst, id, span, tuple).await
        }
        Expr::List(id, span, list) => eval_list(scope, ftable, env, subst, id, span, list).await,
        Expr::Dict(id, span, dict) => eval_dict(scope, ftable, env, subst, id, span, dict).await,
        Expr::Var(var) => eval_var(scope, ftable, env, subst, var).await,
        Expr::App(id, span, f, x) => eval_app(scope, ftable, env, subst, id, span, *f, *x).await,
        Expr::Lam(id, span, param, body) => {
            eval_lam(scope, ftable, env, subst, id, span, param, *body).await
        }
        Expr::Let(id, span, var, def, body) => {
            eval_let(scope, ftable, env, subst, id, span, var, *def, *body).await
        }
        Expr::Ite(id, span, cond, then, r#else) => {
            eval_ite(scope, ftable, env, subst, id, span, *cond, *then, *r#else).await
        }
    }
}

pub async fn eval_tuple(
    scope: &Scope,
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    id: Id,
    span: Span,
    tuple: Vec<Expr>,
) -> Result<Value, Error> {
    let mut result = Vec::with_capacity(tuple.len());
    for v in tuple {
        result.push(eval(scope, ftable, env, subst, v));
    }
    Ok(Value::Expr(Expr::Tuple(
        id,
        span,
        future::join_all(result)
            .await
            .into_iter()
            .map(|v| match v {
                Ok(Value::Expr(e)) => Ok(e),
                Err(e) => Err(e),
                _ => todo!(),
            })
            .collect::<Result<Vec<_>, _>>()?,
    )))
}

pub async fn eval_list(
    scope: &Scope,
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    id: Id,
    span: Span,
    list: Vec<Expr>,
) -> Result<Value, Error> {
    let mut result = Vec::with_capacity(list.len());
    for v in list {
        result.push(eval(scope, ftable, env, subst, v));
    }
    Ok(Value::Expr(Expr::List(
        id,
        span,
        future::join_all(result)
            .await
            .into_iter()
            .map(|v| match v {
                Ok(Value::Expr(e)) => Ok(e),
                Err(e) => Err(e),
                _ => todo!(),
            })
            .collect::<Result<Vec<_>, _>>()?,
    )))
}

pub async fn eval_dict(
    scope: &Scope,
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    id: Id,
    span: Span,
    dict: BTreeMap<String, Expr>,
) -> Result<Value, Error> {
    let mut keys = Vec::with_capacity(dict.len());
    let mut vals = Vec::with_capacity(dict.len());
    for (k, v) in dict {
        keys.push(k);
        vals.push(eval(scope, ftable, env, subst, v));
    }

    let mut result = BTreeMap::new();
    for (k, v) in keys.into_iter().zip(future::join_all(vals).await) {
        result.insert(
            k,
            match v {
                Ok(Value::Expr(e)) => e,
                _ => unimplemented!(),
            },
        );
    }
    Ok(Value::Expr(Expr::Dict(id, span, result)))
}

pub async fn eval_var(
    scope: &Scope,
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    var: Var,
) -> Result<Value, Error> {
    let val = match scope.get(&var.name) {
        Some(val) => val,
        None => &Value::Expr(Expr::Var(var)),
    };

    match val {
        Value::Expr(Expr::Var(var)) => {
            let var_type = env.get(&var.id).unwrap();
            let var_type = unify::apply_subst(var_type, subst);
            let f = ftable.lookup_fns(&var.name, var_type.clone()).next();
            if let Some(f) = f {
                if var_type.num_params() == 0 {
                    return f(ftable, &vec![]).await;
                }
            }
            Ok(Value::Expr(Expr::Var(var.clone())))
        }
        _ => Ok(val.clone()),
    }
}

pub async fn eval_app(
    scope: &Scope,
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    id: Id,
    span: Span,
    f: Expr,
    x: Expr,
) -> Result<Value, Error> {
    let f = eval(scope, ftable, env, subst, f).await?;
    let x = eval(scope, ftable, env, subst, x).await?;

    match f {
        Value::Expr(Expr::Var(var)) => {
            let var_type = env.get(&var.id).unwrap();
            let var_type = unify::apply_subst(var_type, subst);
            let f = ftable.lookup_fns(&var.name, var_type.clone()).next();
            if let Some(f) = f {
                match var_type.num_params() {
                    0 => panic!("Function application on non-function type"),
                    1 => f(ftable, &vec![x]).await,
                    _ => Ok(Value::Closure(var, vec![x])),
                }
            } else {
                panic!("Function not found: {}:{}", var.name, var_type)
            }
        }
        Value::Expr(Expr::Lam(_id, _span, param, body)) => {
            let scope = scope.insert(param.name, x);
            eval(&scope, ftable, env, subst, *body).await
        }
        Value::Closure(var, mut args) => {
            args.push(x);
            let var_type = env.get(&var.id).unwrap();
            let var_type = unify::apply_subst(var_type, subst);
            let f = ftable.lookup_fns(&var.name, var_type.clone()).next();
            if let Some(f) = f {
                if var_type.num_params() < args.len() {
                    panic!("Too many arguments");
                } else if var_type.num_params() == args.len() {
                    f(ftable, &args).await
                } else {
                    Ok(Value::Closure(var, args))
                }
            } else {
                panic!("Function not found: {}", var.name)
            }
        }
        _ => unimplemented!(),
    }
}

pub async fn eval_lam(
    scope: &Scope,
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    id: Id,
    span: Span,
    param: Var,
    body: Expr,
) -> Result<Value, Error> {
    Ok(Value::Expr(Expr::Lam(id, span, param, Box::new(body))))
}

pub async fn eval_let(
    scope: &Scope,
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    id: Id,
    span: Span,
    var: Var,
    def: Expr,
    body: Expr,
) -> Result<Value, Error> {
    let def = eval(scope, ftable, env, subst, def).await?;
    let scope = scope.insert(var.name, def);
    eval(&scope, ftable, env, subst, body).await
}

pub async fn eval_ite(
    scope: &Scope,
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    id: Id,
    span: Span,
    cond: Expr,
    then: Expr,
    r#else: Expr,
) -> Result<Value, Error> {
    let cond = eval(scope, ftable, env, subst, cond).await?;
    match cond {
        Value::Expr(Expr::Bool(_, _, true)) => eval(scope, ftable, env, subst, then).await,
        Value::Expr(Expr::Bool(_, _, false)) => eval(scope, ftable, env, subst, r#else).await,
        _ => unimplemented!(),
    }
}

#[cfg(test)]
pub mod test {
    use std::collections::BTreeSet;

    use rex_ast::id::IdDispenser;
    use rex_lexer::Token;
    use rex_parser::Parser;
    use rex_type_system::{
        arrow,
        constraint::{generate_constraints, Constraint, ConstraintSystem},
        types::{Type, TypeEnv},
        unify::{self},
    };

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

        let res = eval(&Scope::new_sync(), &ftable, &expr_type_env, &subst, expr)
            .await
            .unwrap();
        assert!(matches!(res, Value::Expr(Expr::Uint(_, _, 1))));
    }

    #[tokio::test]
    async fn test_negate() {
        let mut id_dispenser = IdDispenser::new();
        let mut parser = Parser::new(Token::tokenize("negate 3.14").unwrap());
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
        assert!(final_type == Type::Float);

        let res = eval(&Scope::new_sync(), &ftable, &expr_type_env, &subst, expr)
            .await
            .unwrap();
        assert!(matches!(res, Value::Expr(Expr::Float(_, _, -3.14))));
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

        let res = eval(&Scope::new_sync(), &ftable, &expr_type_env, &subst, expr)
            .await
            .unwrap();
        match res {
            Value::Expr(Expr::Tuple(_, _, res)) => {
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

        let res = eval(&Scope::new_sync(), &ftable, &expr_type_env, &subst, expr)
            .await
            .unwrap();
        match res {
            Value::Expr(Expr::List(_, _, res)) => {
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

        let res = eval(&Scope::new_sync(), &ftable, &expr_type_env, &subst, expr)
            .await
            .unwrap();
        assert!(matches!(
            res,
            Value::Expr(Expr::Float(_, _, 11.100000000000001))
        ));
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

        let res = eval(&Scope::new_sync(), &ftable, &expr_type_env, &subst, expr)
            .await
            .unwrap();
        match res {
            Value::Expr(Expr::Tuple(_, _, res)) => {
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

        println!("PARSED: {}", expr);

        let id_op_a0 = id_dispenser.next();

        let mut ftable = Ftable::with_prelude();
        ftable.0.insert(
            "id".to_string(),
            vec![(
                arrow!(Type::Var(id_op_a0) => Type::Var(id_op_a0)),
                Box::new(|_ftable, args| Box::pin(async move { Ok(args[0].clone()) })),
            )],
        );
        let mut type_env = TypeEnv::new();

        let id_op_a0_forall = id_dispenser.next();
        let id_op_type = Type::ForAll(
            id_op_a0_forall,
            Box::new(arrow!(Type::Var(id_op_a0_forall) => Type::Var(id_op_a0_forall))),
            vec![id_op_a0].into_iter().collect::<BTreeSet<_>>(),
        );
        type_env.insert("id".to_string(), id_op_type);

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

        let res = eval(&Scope::new_sync(), &ftable, &expr_type_env, &subst, expr)
            .await
            .unwrap();

        println!("RES: {}", res);

        Ok(())
    }

    #[tokio::test]
    async fn test_let_polymorphism() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser =
            Parser::new(Token::tokenize("let id = \\x -> x in (id 6.9, id 420)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();
        let state = ();

        println!("PARSED: {}", expr);

        let mut ftable = Ftable::with_prelude();
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

        let res = eval(&Scope::new_sync(), &ftable, &expr_type_env, &subst, expr)
            .await
            .unwrap();

        println!("RES: {}", res);

        Ok(())
    }

    #[tokio::test]
    async fn test_let_polymorphism_overloading() -> Result<(), String> {
        let mut id_dispenser = IdDispenser::new();
        let mut parser =
            Parser::new(Token::tokenize("let f = \\x -> negate x in (f 6.9, f 420)").unwrap());
        let expr = parser.parse_expr(&mut id_dispenser).unwrap();

        let mut ftable = Ftable::with_prelude();
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

        let res = eval(&Scope::new_sync(), &ftable, &expr_type_env, &subst, expr).await;

        println!("RES: {:#?}", res);

        Ok(())
    }
}
