use std::{
    collections::{BTreeMap, HashMap},
    fmt::{self, Display, Formatter},
};

use futures::future;
use rex_ast::{
    expr::{Expr, Var},
    id::Id,
};
use rex_type_system::types::Type;

use crate::{
    error::{Error, Trace},
    ftable::Ftable,
    value::{Closure, FunctionLike, Value},
};

pub mod apply;
pub mod error;
pub mod eval;
pub mod ftable;
pub mod value;

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct Context {
    pub vars: HashMap<Id, Value>,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub fn extend(&mut self, rhs: Context) {
        self.vars.extend(rhs.vars)
    }
}

impl Display for Context {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, (k, v)) in self.vars.iter().enumerate() {
            k.fmt(f)?;
            "←".fmt(f)?;
            v.fmt(f)?;
            if i + 1 < self.vars.len() {
                "; ".fmt(f)?;
            }
        }
        Ok(())
    }
}

#[async_recursion::async_recursion]
pub async fn eval<S>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    expr: Expr,
) -> Result<Value, Error>
where
    S: Send + Sync + 'static,
{
    let trace_node = expr.clone();
    match expr {
        Expr::Bool(_id, _span, x) => Ok(Value::Bool(x)),
        Expr::Uint(_id, _span, x) => Ok(Value::Uint(x)),
        Expr::Int(_id, _span, x) => Ok(Value::Int(x)),
        Expr::Float(_id, _span, x) => Ok(Value::Float(x)),
        Expr::String(_id, _span, x) => Ok(Value::String(x)),

        Expr::List(_id, _span, list) => eval_list(ctx, ftable, state, list).await.trace(trace_node),
        Expr::Tuple(_id, _span, tuple) => eval_tuple(ctx, ftable, state, tuple)
            .await
            .trace(trace_node),
        Expr::Dict(_id, _span, dict) => eval_dict(ctx, ftable, state, dict).await.trace(trace_node),

        Expr::Var(var) => eval_var(ctx, ftable, state, var).await.trace(trace_node),
        Expr::App(_id, _span, f, x) => eval_app(ctx, ftable, state, *f, *x).await.trace(trace_node),
        Expr::Lam(id, _span, param, body) => eval_lam(ctx, ftable, id, param, *body)
            .await
            .trace(trace_node),
        Expr::Let(id, _span, var, def, body) => eval_let(ctx, ftable, state, id, var, *def, *body)
            .await
            .trace(trace_node),
        Expr::Ite(_id, _span, cond, then, r#else) => {
            eval_ite(ctx, ftable, state, *cond, *then, *r#else)
                .await
                .trace(trace_node)
        }
    }
}

async fn eval_list<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    list: Vec<Expr>,
) -> Result<Value, Error> {
    let mut result = Vec::with_capacity(list.len());
    for v in list {
        result.push(eval(ctx, ftable, state, v));
    }
    Ok(Value::List(
        future::join_all(result)
            .await
            .into_iter()
            .collect::<Result<_, _>>()?,
    ))
}

async fn eval_tuple<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    tuple: Vec<Expr>,
) -> Result<Value, Error> {
    let mut result = Vec::with_capacity(tuple.len());
    for v in tuple {
        result.push(eval(ctx, ftable, state, v));
    }
    Ok(Value::Tuple(
        future::join_all(result)
            .await
            .into_iter()
            .collect::<Result<_, _>>()?,
    ))
}

async fn eval_dict<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    dict: Vec<(String, Expr)>,
) -> Result<Value, Error> {
    let mut result = BTreeMap::new();
    let mut keys = Vec::with_capacity(dict.len());
    let mut vals = Vec::with_capacity(dict.len());
    for (k, v) in dict {
        keys.push(k);
        vals.push(eval(ctx, ftable, state, v));
    }
    for (k, v) in keys.into_iter().zip(future::join_all(vals).await) {
        result.insert(k, v?);
    }
    Ok(Value::Dict(result))
}

async fn eval_var<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    var: Var,
) -> Result<Value, Error> {
    match ctx.vars.get(&var.id) {
        Some(value) => Ok(value.clone()),
        _ => match ftable.lookup(ctx, &var.id).await {
            Ok(Value::Function(function)) if function.params.is_empty() => {
                // This is a nullary function
                ftable
                    .dispatch(ctx, ftable, state, &function, &vec![])
                    .await
            }
            Ok(v) => Ok(v),
            Err(e) => Err(e),
        },
    }
}

async fn eval_app<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    f: Expr,
    x: Expr,
) -> Result<Value, Error> {
    let (f, x) = future::join(eval(ctx, ftable, state, f), eval(ctx, ftable, state, x)).await;
    apply::apply(ctx, ftable, state, f?, x?).await
}

async fn eval_lam<S: Send + Sync + 'static>(
    ctx: &Context,
    _ftable: &Ftable<S>,
    id: Id,
    param: Var,
    body: Expr,
) -> Result<Value, Error> {
    Ok(Value::Closure(Closure {
        captured_ctx: ctx.clone(),
        captured_args: vec![],
        body: FunctionLike::Lambda(id, param, body),
    }))
}

async fn eval_let<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    _id: Id,
    var: Var,
    def: Expr,
    body: Expr,
) -> Result<Value, Error> {
    let mut new_ctx = ctx.clone();
    new_ctx
        .vars
        .insert(var.id, eval(ctx, ftable, state, def).await?);
    eval(&new_ctx, ftable, state, body).await
}

async fn eval_ite<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    cond: Expr,
    then: Expr,
    r#else: Expr,
) -> Result<Value, Error> {
    let cond = eval(ctx, ftable, state, cond).await?;
    match cond {
        Value::Bool(true) => eval(ctx, ftable, state, then).await,
        Value::Bool(false) => eval(ctx, ftable, state, r#else).await,
        got => Err(Error::UnexpectedType {
            expected: Type::Bool,
            got,
            trace: Default::default(),
        }),
    }
}

#[cfg(test)]
mod test {

    use rex_ast::id::IdDispenser;
    use rex_lexer::Token;
    use rex_parser::Parser;
    use rex_resolver::resolve;
    use rex_type_system::{
        adt, arrow,
        constraint::{self, Constraint},
        types::{ExprTypeEnv, Type, TypeEnv},
        unify::{self, Subst},
    };

    use crate::{
        error::sprint_trace,
        eval,
        value::{Data, DataFields, Function, NamedDataFields, Value},
        Context, Ftable,
    };

    fn ftable_with_point_adt<S: Send + Sync + 'static>(
        id_dispenser: &mut IdDispenser,
    ) -> Ftable<S> {
        let mut ftable = Ftable::with_intrinsics(id_dispenser);
        // register a trivial function that returns 1
        let one_id = id_dispenser.next();
        // ftable.register_function(
        //     Function {
        //         id: one_id,
        //         name: "one".to_string(),
        //         params: vec![],
        //         ret: Type::Uint,
        //     },
        //     Box::new(|_, _, _state: &S, _| Box::pin(async move { Ok(Value::Uint(1)) })),
        // );
        // ftable.register_adt_with_defaults(
        //     id_dispenser,
        //     adt!(
        //         Point =
        //             Point2D { x = Type::Uint, y = Type::Uint } |
        //             Point3D { x = Type::Uint, y = Type::Uint, z = Type::Uint } |
        //             PointI3D { i = Type::Option(Box::new(Type::Uint)), x = Type::Uint, y = Type::Uint, z = Type::Uint }
        //     ),
        //     vec![(
        //         "Point2D".to_string(),
        //         DataFields::Named(NamedDataFields {
        //             fields: vec![
        //                 ("x".to_string(), Value::Id(one_id)),
        //                 ("y".to_string(), Value::Uint(1)),
        //             ]
        //             .into_iter()
        //             .collect(),
        //         }),
        //     ),
        //     (
        //         "PointI3D".to_string(),
        //         DataFields::Named(NamedDataFields {
        //             fields: vec![
        //                 ("x".to_string(), Value::Uint(1)),
        //                 ("y".to_string(), Value::Uint(1)),
        //             ]
        //             .into_iter()
        //             .collect(),
        //         }),
        //     )]
        //     .into_iter()
        //     .collect(),
        // );
        ftable
    }

    //     struct State {
    //         pub foo: u64,
    //     }

    //     #[tokio::test]
    //     async fn state() {
    //         let mut parser = Parser::new(Token::tokenize("getx").unwrap());
    //         let expr = parser.parse_expr().unwrap();
    //         let state = State { foo: 1 };

    //         let mut id_dispenser = parser.id_dispenser;
    //         let mut ftable: Ftable<State> = Ftable::with_intrinsics(&mut id_dispenser);
    //         ftable.register_function(
    //             Function {
    //                 id: id_dispenser.next(),
    //                 name: "getx".to_string(),
    //                 params: vec![],
    //                 ret: Type::Uint,
    //             },
    //             Box::new(|_, _, state: &State, _| Box::pin(async move { Ok(Value::Uint(state.foo)) })),
    //         );

    //         let mut scope = ftable.scope();
    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &state, ast).await.unwrap();

    //         assert_eq!(val, Value::Uint(1))
    //     }

    // #[tokio::test]
    // async fn math() {
    //     let mut parser = Parser::new(Token::tokenize("1 + 2").unwrap());
    //     let expr = parser.parse_expr().unwrap();
    //     let state = ();

    //     let mut id_dispenser = parser.id_dispenser;

    //     let mut ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //     let mut scope = ftable.scope();
    //     let add_op_id = id_dispenser.next();
    //     scope.vars.insert("+".to_string(), add_op_id);

    //     let ctx = Context::new();
    //     let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();

    //     let mut type_env = TypeEnv::new();

    //     let app_op_type_id = id_dispenser.next();
    //     type_env.insert("+".to_string(), Type::Var(app_op_type_id));

    //     let mut type_constraints = vec![Constraint::OneOf(
    //         Type::Var(app_op_type_id),
    //         vec![
    //             arrow!(Type::Uint => arrow!(Type::Uint => Type::Uint)),
    //             arrow!(Type::Int => arrow!(Type::Int => Type::Int)),
    //             arrow!(Type::Float => arrow!(Type::Float => Type::Float)),
    //         ],
    //     )];
    //     let ty = constraint::generate_constraints(
    //         &ast,
    //         &type_env,
    //         &mut ftable.expr_type_env,
    //         &mut type_constraints,
    //         &mut id_dispenser,
    //     )
    //     .unwrap();

    //     let mut subst = Subst::new();
    //     for constraint in &type_constraints {
    //         match constraint {
    //             Constraint::Eq(t1, t2) => unify::unify_eq(t1, t2, &mut subst).unwrap(),
    //             Constraint::OneOf(..) => {}
    //         }
    //     }
    //     for constraint in &type_constraints {
    //         match constraint {
    //             Constraint::Eq(..) => {}
    //             Constraint::OneOf(t1, t2_possibilties) => {
    //                 if t2_possibilties.len() == 1 {
    //                     unify::unify_eq(t1, &t2_possibilties[0], &mut subst).unwrap()
    //                 } else {
    //                     unify::unify_one_of(t1, t2_possibilties, &mut subst).unwrap()
    //                 }
    //             }
    //         }
    //     }

    //     let final_type = unify::apply_subst(&ty, &subst);
    //     println!("EXPR TYPES: {:#?}", ftable.expr_type_env);
    //     println!("EXPRS: {}", ast);
    //     println!("SUBSTS: {:#?}", subst);

    //     let res = eval(&ctx, &ftable, &state, ast).await;

    //     if let Err(e) = &res {
    //         eprintln!("{}", sprint_trace(e.trace()));
    //     }

    //     assert_eq!(res, Ok(Value::Uint(3)))
    // }

    //     #[tokio::test]
    //     async fn negate() {
    //         let mut parser = Parser::new(Token::tokenize("-42").unwrap());
    //         let expr = parser.parse_expr().unwrap();
    //         let state = ();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();
    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &state, ast).await.unwrap();

    //         assert_eq!(val, Value::Int(-42))
    //     }

    //     #[tokio::test]
    //     async fn math_with_precedence() {
    //         let mut parser = Parser::new(Token::tokenize("1 + 2 * 3").unwrap());
    //         let expr = parser.parse_expr().unwrap();
    //         let state = ();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();
    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &state, ast).await.unwrap();

    //         assert_eq!(val, Value::Uint(7));
    //     }

    //     #[tokio::test]
    //     async fn lambda_1() {
    //         let mut parser = Parser::new(Token::tokenize("(\\x -> x) 1").unwrap());
    //         let expr = parser.parse_expr().unwrap();
    //         let state = ();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &state, ast).await.unwrap();

    //         assert_eq!(val, Value::Uint(1));
    //     }

    //     #[tokio::test]
    //     async fn lambda_2() {
    //         let mut parser = Parser::new(Token::tokenize("(\\x y -> x + y) 1 2").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(val, Value::Uint(3));
    //     }

    //     #[tokio::test]
    //     async fn lambda_3() {
    //         let mut parser = Parser::new(Token::tokenize("(\\x y z -> x + y * z) 1 2 3").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(val, Value::Uint(7));
    //     }

    //     #[tokio::test]
    //     async fn let_in() {
    //         let mut parser = Parser::new(Token::tokenize("let x = 1 + 2, y = 3 in x * y").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(val, Value::Uint(9));
    //     }

    //     #[tokio::test]
    //     async fn len() {
    //         let mut parser = Parser::new(Token::tokenize("len [1, 2, 3, 4]").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(val, Value::Uint(4));
    //     }

    //     #[tokio::test]
    //     async fn len_tuple() {
    //         let mut parser = Parser::new(Token::tokenize("len ((1, 2, 3, 4))").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(val, Value::Uint(4));
    //     }

    //     #[tokio::test]
    //     async fn len_str() {
    //         let mut parser = Parser::new(Token::tokenize("len 'asd'").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(val, Value::Uint(3));
    //     }

    //     #[tokio::test]
    //     async fn has() {
    //         let mut parser = Parser::new(Token::tokenize("has 'foo' ({ foo = 1 })").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(val, Value::Bool(true));
    //     }

    //     #[tokio::test]
    //     async fn if_then_else_max() {
    //         let mut parser =
    //             Parser::new(Token::tokenize("(\\x y -> if x > y then x else y) 4 20").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(val, Value::Uint(20));
    //     }

    //     #[tokio::test]
    //     async fn if_then_else_max_with_comment() {
    //         let mut parser =
    //             Parser::new(Token::tokenize("(\\ {- this is the max function -} x y -> if {- check which is bigger -} x > y then x {- x bigger -} else y {- y bigger -} ) 4 20").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(val, Value::Uint(20));
    //     }

    //     #[tokio::test]
    //     async fn test_map() {
    //         let mut parser =
    //             Parser::new(Token::tokenize("map (\\x -> x * 3 + 2 + 1) [1, 2, 3, 4]").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(
    //             val,
    //             Value::List(vec![
    //                 Value::Uint(6),
    //                 Value::Uint(9),
    //                 Value::Uint(12),
    //                 Value::Uint(15)
    //             ])
    //         );
    //     }

    //     #[tokio::test]
    //     async fn test_zip() {
    //         let mut parser = Parser::new(Token::tokenize("zip [1, 2, 3, 4] [4, 3, 2, 1]").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(
    //             val,
    //             Value::List(vec![
    //                 Value::Tuple(vec![Value::Uint(1), Value::Uint(4)]),
    //                 Value::Tuple(vec![Value::Uint(2), Value::Uint(3)]),
    //                 Value::Tuple(vec![Value::Uint(3), Value::Uint(2)]),
    //                 Value::Tuple(vec![Value::Uint(4), Value::Uint(1)]),
    //             ])
    //         );
    //     }

    //     #[tokio::test]
    //     async fn test_filter() {
    //         let mut parser = Parser::new(Token::tokenize("filter ((<) 2) [4, 3, 2, 1]").unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = Ftable::with_intrinsics(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(val, Value::List(vec![Value::Uint(4), Value::Uint(3)]));
    //     }

    //     #[tokio::test]
    //     async fn test_ctor() {
    //         let tokens = Token::tokenize(
    //             "filter (λp → let a = x p, b = y p in a + b == 1)
    //  [Point2D { x = 0, y = 0 },
    //   Point2D { x = 0, y = 1 },
    //   Point3D { x = 1, y = 0, z = 2 },
    //   Point3D { x = 1, y = 1, z = 2 },
    //   PointI3D { i = none, x = 1, y = 0, z = 2 }
    // ]",
    //         )
    //         .unwrap();
    //         let mut parser = Parser::new(tokens);
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = ftable_with_point_adt(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(
    //             val,
    //             Value::List(vec![
    //                 Value::Data(Data {
    //                     name: "Point2D".to_string(),
    //                     fields: Some(DataFields::Named(NamedDataFields {
    //                         fields: vec![
    //                             ("x".to_string(), Value::Uint(0)),
    //                             ("y".to_string(), Value::Uint(1))
    //                         ]
    //                         .into_iter()
    //                         .collect()
    //                     }))
    //                 }),
    //                 Value::Data(Data {
    //                     name: "Point3D".to_string(),
    //                     fields: Some(DataFields::Named(NamedDataFields {
    //                         fields: vec![
    //                             ("x".to_string(), Value::Uint(1)),
    //                             ("y".to_string(), Value::Uint(0)),
    //                             ("z".to_string(), Value::Uint(2))
    //                         ]
    //                         .into_iter()
    //                         .collect()
    //                     }))
    //                 }),
    //                 Value::Data(Data {
    //                     name: "PointI3D".to_string(),
    //                     fields: Some(DataFields::Named(NamedDataFields {
    //                         fields: vec![
    //                             ("i".to_string(), Value::Option(None)),
    //                             ("x".to_string(), Value::Uint(1)),
    //                             ("y".to_string(), Value::Uint(0)),
    //                             ("z".to_string(), Value::Uint(2))
    //                         ]
    //                         .into_iter()
    //                         .collect()
    //                     }))
    //                 }),
    //             ])
    //         );
    //     }

    //     #[tokio::test]
    //     async fn test_ctor_with_defaults() {
    //         let tokens = Token::tokenize("filter (λp → let a = x p, b = y p in a + b == 1) [Point2D { x = 0, y = 0 }, Point2D { x = 0 }, Point2D {}, Point3D { x = let a = 1, b = 1 in a * b, y = 0, z = let a = 1, b = 1 in a + b }, Point3D { x = 1, y = 1, z = 2 }]").unwrap();
    //         let mut parser = Parser::new(tokens);
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = ftable_with_point_adt(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

    //         assert_eq!(
    //             val,
    //             Value::List(vec![
    //                 Value::Data(Data {
    //                     name: "Point2D".to_string(),
    //                     fields: Some(DataFields::Named(NamedDataFields {
    //                         fields: vec![
    //                             ("x".to_string(), Value::Uint(0)),
    //                             ("y".to_string(), Value::Uint(1))
    //                         ]
    //                         .into_iter()
    //                         .collect()
    //                     }))
    //                 }),
    //                 Value::Data(Data {
    //                     name: "Point3D".to_string(),
    //                     fields: Some(DataFields::Named(NamedDataFields {
    //                         fields: vec![
    //                             ("x".to_string(), Value::Uint(1)),
    //                             ("y".to_string(), Value::Uint(0)),
    //                             ("z".to_string(), Value::Uint(2))
    //                         ]
    //                         .into_iter()
    //                         .collect()
    //                     }))
    //                 }),
    //             ])
    //         );
    //     }

    //     #[tokio::test]
    //     async fn test_10k_ctor_with_defaults() {
    //         // create an expression that builds 1000 points, maps them, and then filters them
    //         let mut expr =
    //             "filter (λp → let a = x p, b = y p in a + b == 1) (map (λi → i)  [".to_string();
    //         for i in 0..10_000 {
    //             expr += &format!("Point2D {{ x = {}, y = {} }}", i % 10, i / 10 % 10);
    //             if i + 1 < 10_000 {
    //                 expr += ", ";
    //             }
    //         }

    //         expr += "])";

    //         let mut parser = Parser::new(Token::tokenize(&expr).unwrap());
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = ftable_with_point_adt(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await;
    //         match val {
    //             Ok(val) => {
    //                 if let Value::List(val) = val {
    //                     assert_eq!(val.len(), 200);
    //                 } else {
    //                     panic!("expected Value::List, got {:?}", val);
    //                 }
    //             }
    //             Err(e) => {
    //                 eprintln!("{}", e);
    //                 panic!("{}", sprint_trace(e.trace()));
    //             }
    //         }
    //     }

    //     #[tokio::test]
    //     async fn test_bad_ctor() {
    //         let tokens = Token::tokenize("filter (λp → let a = x p, b = y p in a + b + true == 1) [Point2D { x = 0, y = 0 }, Point2D { x = 0, y = 1 }, Point3D { x = 1, y = 0, z = 2 }, Point3D { x = 1, y = 1, z = 2 }]").unwrap();
    //         let mut parser = Parser::new(tokens);
    //         let expr = parser.parse_expr().unwrap();

    //         let mut id_dispenser = parser.id_dispenser;
    //         let ftable = ftable_with_point_adt(&mut id_dispenser);

    //         let mut scope = ftable.scope();

    //         let ctx = Context::new();
    //         let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
    //         let val = eval(&ctx, &ftable, &(), ast).await;

    //         match val {
    //             Ok(_) => unreachable!(),
    //             Err(e) => {
    //                 assert_eq!(format!("{}", e), "expected uint, got `true`");
    //                 assert_eq!(
    //                     sprint_trace(e.trace()),
    //                     r#"filter (λp → let a = x p in let b = y p in (==) ((+) ((+) a b) true) 1) [Point2D {x = 0, y = 0}, Point2D {x = 0, y = 1}, Point3D {x = 1, y = 0, z = 2}, Point3D {x = 1, y = 1, z = 2}]
    //   let a = x p in let b = y p in (==) ((+) ((+) a b) true) 1
    //     let b = y p in (==) ((+) ((+) a b) true) 1
    //       (==) ((+) ((+) a b) true) 1
    //         (==) ((+) ((+) a b) true)
    //           (+) ((+) a b) true
    // "#
    //                 );
    //             }
    //         }
    //     }
}
