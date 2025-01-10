use std::{collections::HashMap, future::Future, pin::Pin};

use futures::future;
use rex_ast::{
    expr::{Expr, Var},
    id::Id,
};
use rex_lexer::span::Span;
use rex_type_system::{
    types::{ExprTypeEnv, Type, TypeEnv},
    unify::{self, Subst},
};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Extern {
    pub name: String,
    pub t: Type,
}

#[derive(Clone, Debug)]
pub enum Value {
    Expr(Expr),
    Closure(Var, Vec<Value>),
}

pub trait F<'r>:
    Fn(&'r Ftable, &'r Vec<Value>) -> Pin<Box<dyn Future<Output = Value> + Send + 'r>> + Sync + Send
{
    fn clone_box(&self) -> FtableFn;
}

impl<'r, G> F<'r> for G
where
    for<'q> G: Fn(&'q Ftable, &'q Vec<Value>) -> Pin<Box<dyn Future<Output = Value> + Send + 'q>>
        + Sync
        + Send
        + Clone
        + 'r + 'q,
{
    fn clone_box(&self) -> FtableFn {
        Box::new((*self).clone())
    }
}

pub type FtableFn = Box<dyn for<'r> F<'r>>;

#[derive(Clone)]
pub struct Ftable(HashMap<(String, Type), FtableFn>);

impl Clone for Box<dyn for<'r> F<'r>> {
    fn clone(&self) -> Self {
        (**self).clone_box()
    }
}

#[async_recursion::async_recursion]
pub async fn eval(ftable: &Ftable, env: &ExprTypeEnv, subst: &Subst, expr: Expr) -> Value {
    match expr {
        Expr::Bool(..) => Value::Expr(expr),
        Expr::Uint(..) => Value::Expr(expr),
        Expr::Int(..) => Value::Expr(expr),
        Expr::Float(..) => Value::Expr(expr),
        Expr::String(..) => Value::Expr(expr),
        Expr::Tuple(id, span, tuple) => eval_tuple(ftable, env, subst, id, span, tuple).await,
        Expr::List(id, span, list) => eval_list(ftable, env, subst, id, span, list).await,
        Expr::Var(..) => Value::Expr(expr),
        Expr::App(id, span, f, x) => eval_app(ftable, env, subst, id, span, *f, *x).await,
        Expr::Lam(..) => unimplemented!(),
        Expr::Let(id, span, var, def, body) => {
            eval_let(ftable, env, subst, id, span, var, *def, *body).await
        }
        Expr::Ite(..) => unimplemented!(),
        _ => unimplemented!(),
    }
}

pub async fn eval_tuple(
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    id: Id,
    span: Span,
    tuple: Vec<Expr>,
) -> Value {
    let mut result = Vec::with_capacity(tuple.len());
    for v in tuple {
        result.push(eval(ftable, env, subst, v));
    }
    Value::Expr(Expr::Tuple(
        id,
        span,
        future::join_all(result)
            .await
            .into_iter()
            .map(|v| match v {
                Value::Expr(e) => e,
                _ => unimplemented!(),
            })
            .collect(),
    ))
}

pub async fn eval_list(
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    id: Id,
    span: Span,
    list: Vec<Expr>,
) -> Value {
    let mut result = Vec::with_capacity(list.len());
    for v in list {
        result.push(eval(ftable, env, subst, v));
    }
    Value::Expr(Expr::List(
        id,
        span,
        future::join_all(result)
            .await
            .into_iter()
            .map(|v| match v {
                Value::Expr(e) => e,
                _ => unimplemented!(),
            })
            .collect(),
    ))
}

pub async fn eval_app(
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    id: Id,
    span: Span,
    f: Expr,
    x: Expr,
) -> Value {
    let f = eval(ftable, env, subst, f).await;
    let x = eval(ftable, env, subst, x).await;

    match f {
        Value::Expr(Expr::Var(var)) => {
            let var_type = env.get(&var.id).unwrap();
            let var_type = unify::apply_subst(var_type, subst);
            let f = ftable.0.get(&(var.name.clone(), var_type.clone()));
            if let Some(f) = f {
                match var_type.num_params() {
                    0 => panic!("Function application on non-function type"),
                    1 => f(ftable, &vec![x]).await,
                    _ => Value::Closure(var, vec![x]),
                }
            } else {
                panic!("Function not found: {}:{}", var.name, var_type)
            }
        }
        Value::Closure(var, mut args) => {
            args.push(x);
            let var_type = env.get(&var.id).unwrap();
            let var_type = unify::apply_subst(var_type, subst);
            let f = ftable.0.get(&(var.name.clone(), var_type.clone()));
            if let Some(f) = f {
                if var_type.num_params() < args.len() {
                    panic!("Too many arguments");
                } else if var_type.num_params() == args.len() {
                    f(ftable, &args).await
                } else {
                    Value::Closure(var, args)
                }
            } else {
                panic!("Function not found: {}", var.name)
            }
        }
        _ => unimplemented!(),
    }
}

pub async fn eval_let(
    ftable: &Ftable,
    env: &ExprTypeEnv,
    subst: &Subst,
    id: Id,
    span: Span,
    var: Var,
    def: Expr,
    body: Expr,
) -> Value {
    unimplemented!()
}

#[cfg(test)]
pub mod test {
    use rex_lexer::Token;
    use rex_parser::Parser;
    use rex_resolver::Scope;
    use rex_type_system::{
        arrow,
        constraint::{self, generate_constraints, Constraint, ConstraintSystem},
        trace::{sprint_expr_with_type, sprint_subst, sprint_type_env},
        types::TypeEnv,
        unify::{self, Subst},
    };

    use super::*;

    #[tokio::test]
    async fn test_simple() {
        let mut parser = Parser::new(Token::tokenize("1").unwrap());
        let expr = parser.parse_expr().unwrap();
        let state = ();

        let mut id_dispenser = parser.id_dispenser;

        let mut ftable = Ftable(Default::default());

        let mut scope = Scope::default();
        let add_op_id = id_dispenser.next();
        scope.vars.insert("+".to_string(), add_op_id);

        let mut type_env = TypeEnv::new();

        let app_op_type_id = id_dispenser.next();
        // type_env.insert("+".to_string(), Type::Var(app_op_type_id));

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

        let mut subst = Subst::new();
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst).unwrap(),
                Constraint::OneOf(..) => {}
            }
        }
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(..) => {}
                Constraint::OneOf(t1, t2_possibilties) => {
                    if t2_possibilties.len() == 1 {
                        unify::unify_eq(t1, &t2_possibilties[0], &mut subst).unwrap()
                    } else {
                        unify::unify_one_of(t1, t2_possibilties, &mut subst).unwrap()
                    }
                }
            }
        }

        let final_type = unify::apply_subst(&ty, &subst);
        println!("EXPR TYPES: {:#?}", expr_type_env);
        println!("EXPRS: {:#?}", expr);
        println!("SUBSTS: {:#?}", subst);

        let res = eval(&ftable, &expr_type_env, &subst, expr).await;

        println!("RES: {:#?}", res);
    }

    #[tokio::test]
    async fn test_negate() {
        let mut parser = Parser::new(Token::tokenize("- 3.14").unwrap());
        let expr = parser.parse_expr().unwrap();
        let state = ();

        let mut id_dispenser = parser.id_dispenser;

        let mut ftable = Ftable(Default::default());

        let mut scope = Scope::default();
        let neg_op_id = id_dispenser.next();
        scope.vars.insert("negate".to_string(), neg_op_id);

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
                ],
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

        let mut subst = Subst::new();
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst).unwrap(),
                Constraint::OneOf(..) => {}
            }
        }
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(..) => {}
                Constraint::OneOf(t1, t2_possibilties) => {
                    if t2_possibilties.len() == 1 {
                        unify::unify_eq(t1, &t2_possibilties[0], &mut subst).unwrap()
                    } else {
                        unify::unify_one_of(t1, t2_possibilties, &mut subst);
                    }
                }
            }
        }

        let final_type = unify::apply_subst(&ty, &subst);
        println!("EXPR TYPES: {:#?}", expr_type_env);
        println!("EXPRS: {:#?}", expr);
        println!("SUBSTS: {:#?}", subst);

        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Uint => Type::Int)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Uint(id, span, x)) => {
                            Value::Expr(Expr::Int(*id, *span, -(*x as i64)))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Int => Type::Int)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Int(id, span, x)) => {
                            Value::Expr(Expr::Int(*id, *span, -*x))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Float => Type::Float)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Float(id, span, x)) => {
                            Value::Expr(Expr::Float(*id, *span, -*x))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        let res = eval(&ftable, &expr_type_env, &subst, expr).await;

        println!("RES: {:#?}", res);
    }

    #[tokio::test]
    async fn test_negate_tuple() {
        let mut parser = Parser::new(Token::tokenize("(negate 6.9, negate 420)").unwrap());
        let expr = parser.parse_expr().unwrap();
        let state = ();

        println!("PARSED: {}", expr);

        let mut id_dispenser = parser.id_dispenser;

        let mut ftable = Ftable(Default::default());

        let mut scope = Scope::default();
        let neg_op_id = id_dispenser.next();
        scope.vars.insert("negate".to_string(), neg_op_id);

        // let expr = resolve(&mut id_dispenser, &mut scope, expr).unwrap();

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
                ],
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

        let mut subst = Subst::new();
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst).unwrap(),
                Constraint::OneOf(..) => {}
            }
        }
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(..) => {}
                Constraint::OneOf(t1, t2_possibilties) => {
                    if t2_possibilties.len() == 1 {
                        unify::unify_eq(t1, &t2_possibilties[0], &mut subst);
                    } else {
                        unify::unify_one_of(t1, t2_possibilties, &mut subst);
                    }
                }
            }
        }

        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "EXPR: {}\nCONSTRAINTS: {}\nSUBST: {}",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst)),
            &constraint_system,
            sprint_subst(&subst)
        );

        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Uint => Type::Int)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Uint(id, span, x)) => {
                            Value::Expr(Expr::Int(*id, *span, -(*x as i64)))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Int => Type::Int)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Int(id, span, x)) => {
                            Value::Expr(Expr::Int(*id, *span, -*x))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Float => Type::Float)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Float(id, span, x)) => {
                            Value::Expr(Expr::Float(*id, *span, -*x))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        let res = eval(&ftable, &expr_type_env, &subst, expr).await;

        println!("RES: {:#?}", res);
    }

    #[tokio::test]
    async fn test_negate_list() {
        let mut parser = Parser::new(Token::tokenize("[-6.9, -3.14]").unwrap());
        let expr = parser.parse_expr().unwrap();
        let state = ();

        println!("PARSED: {}", expr);

        let mut id_dispenser = parser.id_dispenser;

        let mut ftable = Ftable(Default::default());

        let mut scope = Scope::default();
        let neg_op_id = id_dispenser.next();
        scope.vars.insert("negate".to_string(), neg_op_id);

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
                ],
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

        let mut subst = Subst::new();
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst).unwrap(),
                Constraint::OneOf(..) => {}
            }
        }
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(..) => {}
                Constraint::OneOf(t1, t2_possibilties) => {
                    if t2_possibilties.len() == 1 {
                        unify::unify_eq(t1, &t2_possibilties[0], &mut subst);
                    } else {
                        unify::unify_one_of(t1, t2_possibilties, &mut subst);
                    }
                }
            }
        }

        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "EXPR: {}\nCONSTRAINTS: {}\nSUBST: {}",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst)),
            &constraint_system,
            sprint_subst(&subst)
        );

        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Uint => Type::Int)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Uint(id, span, x)) => {
                            Value::Expr(Expr::Int(*id, *span, -(*x as i64)))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Int => Type::Int)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Int(id, span, x)) => {
                            Value::Expr(Expr::Int(*id, *span, -*x))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Float => Type::Float)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Float(id, span, x)) => {
                            Value::Expr(Expr::Float(*id, *span, -*x))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        let res = eval(&ftable, &expr_type_env, &subst, expr).await;

        println!("RES: {:#?}", res);
    }

    #[tokio::test]
    async fn test_add() {
        let mut parser = Parser::new(Token::tokenize("6.9 + 4.20").unwrap());
        let expr = parser.parse_expr().unwrap();
        let state = ();

        let mut id_dispenser = parser.id_dispenser;

        let mut ftable = Ftable(Default::default());

        let mut scope = Scope::default();
        let neg_op_id = id_dispenser.next();
        scope.vars.insert("+".to_string(), neg_op_id);

        let mut type_env = TypeEnv::new();

        let neg_op_typeid = id_dispenser.next();
        type_env.insert("+".to_string(), Type::Var(neg_op_typeid));

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
                Type::Var(neg_op_typeid),
                vec![
                    arrow!(Type::Uint => arrow!(Type::Uint =>  Type::Uint)),
                    arrow!(Type::Int => arrow!(Type::Int =>  Type::Int)),
                    arrow!(Type::Float => arrow!(Type::Float => Type::Float)),
                ],
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

        let mut subst = Subst::new();
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst).unwrap(),
                Constraint::OneOf(..) => {}
            }
        }
        for constraint in constraint_system.constraints() {
            match constraint {
                Constraint::Eq(..) => {}
                Constraint::OneOf(t1, t2_possibilties) => {
                    if t2_possibilties.len() == 1 {
                        unify::unify_eq(t1, &t2_possibilties[0], &mut subst);
                    } else {
                        unify::unify_one_of(t1, t2_possibilties, &mut subst);
                    }
                }
            }
        }

        let final_type = unify::apply_subst(&ty, &subst);
        println!("EXPR TYPES: {:#?}", expr_type_env);
        println!("EXPRS: {:#?}", expr);
        println!("SUBSTS: {:#?}", subst);

        ftable.0.insert(
            (
                "+".to_string(),
                arrow!(Type::Uint => arrow!(Type::Uint => Type::Uint)),
            ),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match (&args[0], &args[1]) {
                        (
                            Value::Expr(Expr::Uint(id1, span1, x1)),
                            Value::Expr(Expr::Uint(id2, span2, x2)),
                        ) => Value::Expr(Expr::Uint(
                            *id1,
                            Span::from_begin_end(span1.begin, span2.end),
                            x1 + x2,
                        )),
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            (
                "+".to_string(),
                arrow!(Type::Int => arrow!(Type::Int => Type::Int)),
            ),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match (&args[0], &args[1]) {
                        (
                            Value::Expr(Expr::Int(id1, span1, x1)),
                            Value::Expr(Expr::Int(id2, span2, x2)),
                        ) => Value::Expr(Expr::Int(
                            *id1,
                            Span::from_begin_end(span1.begin, span2.end),
                            x1 + x2,
                        )),
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            (
                "+".to_string(),
                arrow!(Type::Float => arrow!(Type::Float => Type::Float)),
            ),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match (&args[0], &args[1]) {
                        (
                            Value::Expr(Expr::Float(id1, span1, x1)),
                            Value::Expr(Expr::Float(id2, span2, x2)),
                        ) => Value::Expr(Expr::Float(
                            *id1,
                            Span::from_begin_end(span1.begin, span2.end),
                            x1 + x2,
                        )),
                        _ => unreachable!(),
                    }
                })
            }),
        );
        let res = eval(&ftable, &expr_type_env, &subst, expr).await;

        println!("RES: {:#?}", res);
    }

    #[tokio::test]
    async fn test_add_tuple() {
        let mut parser = Parser::new(Token::tokenize("(6.9 + 4.20, 6 + 9)").unwrap());
        let expr = parser.parse_expr().unwrap();
        let state = ();

        let mut id_dispenser = parser.id_dispenser;

        let mut scope = Scope::default();
        let add_op_id = id_dispenser.next();
        scope.vars.insert("+".to_string(), add_op_id);

        let mut type_env = TypeEnv::new();

        let add_op_typeid = id_dispenser.next();
        type_env.insert("+".to_string(), Type::Var(add_op_typeid));

        let mut constraint_system =
            ConstraintSystem::with_global_constraints(vec![Constraint::OneOf(
                Type::Var(add_op_typeid),
                vec![
                    arrow!(Type::Uint => arrow!(Type::Uint =>  Type::Uint)),
                    arrow!(Type::Int => arrow!(Type::Int =>  Type::Int)),
                    arrow!(Type::Float => arrow!(Type::Float => Type::Float)),
                ],
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

        let mut subst = Subst::new();
        for _ in 1..10 {
            for constraint in constraint_system.constraints() {
                match constraint {
                    Constraint::Eq(t1, t2) => {
                        unify::unify_eq(t1, t2, &mut subst);
                    }
                    Constraint::OneOf(..) => {}
                }
            }
            for constraint in constraint_system.constraints() {
                match constraint {
                    Constraint::Eq(..) => {}
                    Constraint::OneOf(t1, t2_possibilties) => {
                        unify::unify_one_of(t1, t2_possibilties, &mut subst);
                    }
                }
            }
        }

        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "{}\n{}",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst)),
            &constraint_system
        );

        let mut ftable = Ftable(Default::default());
        ftable.0.insert(
            (
                "+".to_string(),
                arrow!(Type::Uint => arrow!(Type::Uint => Type::Uint)),
            ),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match (&args[0], &args[1]) {
                        (
                            Value::Expr(Expr::Uint(id1, span1, x1)),
                            Value::Expr(Expr::Uint(id2, span2, x2)),
                        ) => Value::Expr(Expr::Uint(
                            *id1,
                            Span::from_begin_end(span1.begin, span2.end),
                            x1 + x2,
                        )),
                        _ => panic!("Expected Uint -> Uint -> Uint, got {:#?}", args),
                    }
                })
            }),
        );
        ftable.0.insert(
            (
                "+".to_string(),
                arrow!(Type::Int => arrow!(Type::Int => Type::Int)),
            ),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match (&args[0], &args[1]) {
                        (
                            Value::Expr(Expr::Int(id1, span1, x1)),
                            Value::Expr(Expr::Int(id2, span2, x2)),
                        ) => Value::Expr(Expr::Int(
                            *id1,
                            Span::from_begin_end(span1.begin, span2.end),
                            x1 + x2,
                        )),
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            (
                "+".to_string(),
                arrow!(Type::Float => arrow!(Type::Float => Type::Float)),
            ),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match (&args[0], &args[1]) {
                        (
                            Value::Expr(Expr::Float(id1, span1, x1)),
                            Value::Expr(Expr::Float(id2, span2, x2)),
                        ) => Value::Expr(Expr::Float(
                            *id1,
                            Span::from_begin_end(span1.begin, span2.end),
                            x1 + x2,
                        )),
                        _ => unreachable!(),
                    }
                })
            }),
        );
        let res = eval(&ftable, &expr_type_env, &subst, expr).await;

        println!("RES: {:#?}", res);
    }

    #[tokio::test]
    async fn test_let_polymorphism() {
        let mut parser =
            Parser::new(Token::tokenize("let id = \\x -> x in (id 6.9, id 420)").unwrap());
        let expr = parser.parse_expr().unwrap();
        let state = ();

        println!("PARSED: {}", expr);

        let mut id_dispenser = parser.id_dispenser;

        let mut ftable = Ftable(Default::default());

        let mut scope = Scope::default();
        let neg_op_id = id_dispenser.next();
        scope.vars.insert("negate".to_string(), neg_op_id);

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
                ],
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

        let mut subst = Subst::new();
        for _ in 1..10 {
            for constraint in constraint_system.constraints() {
                match constraint {
                    Constraint::Eq(t1, t2) => {
                        unify::unify_eq(t1, t2, &mut subst);
                    }
                    Constraint::OneOf(..) => {}
                }
            }
            for constraint in constraint_system.constraints() {
                match constraint {
                    Constraint::Eq(..) => {}
                    Constraint::OneOf(t1, t2_possibilties) => {
                        unify::unify_one_of(t1, t2_possibilties, &mut subst);
                    }
                }
            }
        }

        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "EXPR: {}\nCONSTRAINTS: {}\nSUBST: {}",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst)),
            &constraint_system,
            sprint_subst(&subst)
        );

        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Uint => Type::Int)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Uint(id, span, x)) => {
                            Value::Expr(Expr::Int(*id, *span, -(*x as i64)))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Int => Type::Int)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Int(id, span, x)) => {
                            Value::Expr(Expr::Int(*id, *span, -*x))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Float => Type::Float)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Float(id, span, x)) => {
                            Value::Expr(Expr::Float(*id, *span, -*x))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        let res = eval(&ftable, &expr_type_env, &subst, expr).await;

        println!("RES: {:#?}", res);
    }

    #[tokio::test]
    async fn test_let_polymorphism_overloading() {
        let mut parser =
            Parser::new(Token::tokenize("let f = \\x -> negate x in (f 6.9, f 420)").unwrap());
        let expr = parser.parse_expr().unwrap();
        let state = ();

        println!("PARSED: {}\n", expr);

        let mut id_dispenser = parser.id_dispenser;

        let mut ftable = Ftable(Default::default());

        let mut scope = Scope::default();
        let neg_op_id = id_dispenser.next();
        scope.vars.insert("negate".to_string(), neg_op_id);

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
                ],
            )]);
        let mut expr_type_env = ExprTypeEnv::new();

        println!(
            "EXPR: {}\nTYPE_ENV: {}\nCONSTRAINTS: {}\n",
            sprint_expr_with_type(&expr, &expr_type_env, None),
            sprint_type_env(&type_env),
            &constraint_system,
        );

        let ty = generate_constraints(
            &expr,
            &type_env,
            &mut expr_type_env,
            &mut constraint_system,
            &mut id_dispenser,
        )
        .unwrap();

        let mut subst = Subst::new();
        for _ in 1..10 {
            for constraint in constraint_system.constraints() {
                match constraint {
                    Constraint::Eq(t1, t2) => {
                        unify::unify_eq(t1, t2, &mut subst);
                    }
                    Constraint::OneOf(..) => {}
                }
            }
            for constraint in constraint_system.constraints() {
                match constraint {
                    Constraint::Eq(..) => {}
                    Constraint::OneOf(t1, t2_possibilties) => {
                        unify::unify_one_of(t1, t2_possibilties, &mut subst);
                    }
                }
            }
        }

        let final_type = unify::apply_subst(&ty, &subst);

        println!(
            "EXPR: {}\nCONSTRAINTS: {}\nSUBST: {}",
            sprint_expr_with_type(&expr, &expr_type_env, Some(&subst)),
            &constraint_system,
            sprint_subst(&subst)
        );

        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Uint => Type::Int)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Uint(id, span, x)) => {
                            Value::Expr(Expr::Int(*id, *span, -(*x as i64)))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Int => Type::Int)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Int(id, span, x)) => {
                            Value::Expr(Expr::Int(*id, *span, -*x))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        ftable.0.insert(
            ("negate".to_string(), arrow!(Type::Float => Type::Float)),
            Box::new(|_ftable, args| {
                Box::pin(async move {
                    match &args[0] {
                        Value::Expr(Expr::Float(id, span, x)) => {
                            Value::Expr(Expr::Float(*id, *span, -*x))
                        }
                        _ => unreachable!(),
                    }
                })
            }),
        );
        let res = eval(&ftable, &expr_type_env, &subst, expr).await;

        println!("RES: {:#?}", res);
    }
}
