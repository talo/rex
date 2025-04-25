use std::collections::BTreeMap;

pub use context::Context;
use futures::future;
use rex_ast::{
    ast::{Call, Ctor, Fields, IfThenElse, Lambda, LetIn, NamedFields, UnnamedFields, Var, AST},
    types::Type,
};

use crate::{
    error::{Error, Trace},
    ftable::Ftable,
    value::{Closure, Data, DataFields, FunctionLike, NamedDataFields, UnnamedDataFields, Value},
};

pub mod apply;
pub mod context;
pub mod error;
pub mod ftable;
#[cfg(feature = "stats")]
pub mod stats;
pub mod value;

#[async_recursion::async_recursion]
pub async fn eval<S>(ctx: &Context, ftable: &Ftable<S>, state: &S, ast: AST) -> Result<Value, Error>
where
    S: Send + Sync + 'static,
{
    let trace_node = ast.clone();
    match ast {
        AST::Null(_span) => Ok(Value::Null),
        AST::Bool(_span, x) => Ok(Value::Bool(x)),
        AST::Uint(_span, x) => Ok(Value::Uint(x)),
        AST::Int(_span, x) => Ok(Value::Int(x)),
        AST::Float(_span, x) => Ok(Value::Float(x)),
        AST::String(_span, x) => Ok(Value::String(x)),

        AST::List(_span, list) => eval_list(ctx, ftable, state, list).await.trace(trace_node),
        AST::Tuple(_span, tuple) => eval_tuple(ctx, ftable, state, tuple)
            .await
            .trace(trace_node),
        AST::Dict(_span, dict) => eval_dict(ctx, ftable, state, dict).await.trace(trace_node),

        AST::Var(var) => eval_var(ctx, ftable, state, var).await.trace(trace_node),
        AST::Call(call) => eval_call(ctx, ftable, state, call).await.trace(trace_node),
        AST::Lambda(lam) => eval_lambda(ctx, ftable, lam).await.trace(trace_node),
        AST::LetIn(let_in) => eval_let_in(ctx, ftable, state, let_in)
            .await
            .trace(trace_node),
        AST::IfThenElse(ite) => eval_ite(ctx, ftable, state, ite).await.trace(trace_node),
        AST::Ctor(ctor) => eval_ctor(ctx, ftable, state, ctor).await.trace(trace_node),
    }
}

async fn eval_list<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    list: Vec<AST>,
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
    tuple: Vec<AST>,
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
    dict: Vec<(Var, AST)>,
) -> Result<Value, Error> {
    let mut result = BTreeMap::new();
    let mut keys = Vec::with_capacity(dict.len());
    let mut vals = Vec::with_capacity(dict.len());
    for (k, v) in dict {
        keys.push(k.name);
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
    match ctx.get(&var.id) {
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

async fn eval_call<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    call: Call,
) -> Result<Value, Error> {
    let (base, arg) = future::join(
        eval(ctx, ftable, state, *call.base),
        eval(ctx, ftable, state, *call.arg),
    )
    .await;
    apply::apply(ctx, ftable, state, base?, arg?).await
}

async fn eval_lambda<S: Send + Sync + 'static>(
    ctx: &Context,
    _ftable: &Ftable<S>,
    lam: Lambda,
) -> Result<Value, Error> {
    Ok(Value::Closure(Closure {
        captured_ctx: ctx.clone(),
        captured_args: vec![],
        body: FunctionLike::Lambda(lam),
    }))
}

async fn eval_let_in<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    let_in: LetIn,
) -> Result<Value, Error> {
    let mut new_ctx = ctx.clone();
    new_ctx.insert(let_in.var.id, eval(ctx, ftable, state, *let_in.def).await?);
    eval(&new_ctx, ftable, state, *let_in.body).await
}

async fn eval_ite<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    ite: IfThenElse,
) -> Result<Value, Error> {
    let cond = eval(ctx, ftable, state, *ite.cond).await?;
    match cond {
        Value::Bool(true) => eval(ctx, ftable, state, *ite.then).await,
        Value::Bool(false) => eval(ctx, ftable, state, *ite.r#else).await,
        got => Err(Error::UnexpectedType {
            expected: Type::Bool,
            got,
            trace: Default::default(),
        }),
    }
}

async fn eval_ctor<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    ctor: Ctor,
) -> Result<Value, Error> {
    Ok(Value::Data(Data {
        name: ctor.name,
        fields: Some(eval_fields(ctx, ftable, state, ctor.fields).await?),
    }))
}

async fn eval_fields<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    fields: Fields,
) -> Result<DataFields, Error> {
    match fields {
        Fields::Named(named_fields) => Ok(DataFields::Named(
            eval_named_fields(ctx, ftable, state, named_fields).await?,
        )),
        Fields::Unnamed(unnamed_fields) => Ok(DataFields::Unnamed(
            eval_unnamed_fields(ctx, ftable, state, unnamed_fields).await?,
        )),
    }
}

async fn eval_named_fields<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    named_fields: NamedFields,
) -> Result<NamedDataFields, Error> {
    let mut result = BTreeMap::new();
    let mut keys = Vec::with_capacity(named_fields.fields.len());
    let mut vals = Vec::with_capacity(named_fields.fields.len());
    for (k, v) in named_fields.fields {
        keys.push(k.name);
        vals.push(eval(ctx, ftable, state, v));
    }
    for (k, v) in keys.into_iter().zip(future::join_all(vals).await) {
        result.insert(k, v?);
    }
    Ok(NamedDataFields { fields: result })
}

async fn eval_unnamed_fields<S: Send + Sync + 'static>(
    ctx: &Context,
    ftable: &Ftable<S>,
    state: &S,
    unnamed_fields: UnnamedFields,
) -> Result<UnnamedDataFields, Error> {
    let mut result = Vec::with_capacity(unnamed_fields.fields.len());
    for v in unnamed_fields.fields {
        result.push(eval(ctx, ftable, state, v));
    }
    Ok(UnnamedDataFields {
        fields: future::join_all(result)
            .await
            .into_iter()
            .collect::<Result<_, _>>()?,
    })
}

#[cfg(test)]
mod test {
    use rex_ast::{adt, adt_variant_with_named_fields, id::IdDispenser, types::Type};
    use rex_lexer::Token;
    use rex_parser::Parser;
    use rex_resolver::resolve;

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
        ftable.register_function(
            Function {
                id: one_id,
                name: "one".to_string(),
                params: vec![],
                ret: Type::Uint,
            },
            Box::new(|_, _, _state: &S, _| Box::pin(async move { Ok(Value::Uint(1)) })),
        );
        ftable.register_adt_with_defaults(
            id_dispenser,
            adt!(
                Point =
                    Point2D { x = Type::Uint, y = Type::Uint } |
                    Point3D { x = Type::Uint, y = Type::Uint, z = Type::Uint } |
                    PointI3D { i = Type::Option(Box::new(Type::Uint)), x = Type::Uint, y = Type::Uint, z = Type::Uint }
            ),
            vec![(
                "Point2D".to_string(),
                DataFields::Named(NamedDataFields {
                    fields: vec![
                        ("x".to_string(), Value::Id(one_id)),
                        ("y".to_string(), Value::Uint(1)),
                    ]
                    .into_iter()
                    .collect(),
                }),
            ),
            (
                "PointI3D".to_string(),
                DataFields::Named(NamedDataFields {
                    fields: vec![
                        ("x".to_string(), Value::Uint(1)),
                        ("y".to_string(), Value::Uint(1)),
                    ]
                    .into_iter()
                    .collect(),
                }),
            )]
            .into_iter()
            .collect(),
        );
        ftable
    }

    struct State {
        pub foo: u64,
    }

    #[tokio::test]
    async fn state() {
        let mut parser = Parser::new(Token::tokenize("getx").unwrap());
        let expr = parser.parse_expr().unwrap();
        let state = State { foo: 1 };

        let mut id_dispenser = parser.id_dispenser;
        let mut ftable: Ftable<State> = Ftable::with_intrinsics(&mut id_dispenser);
        ftable.register_function(
            Function {
                id: id_dispenser.next(),
                name: "getx".to_string(),
                params: vec![],
                ret: Type::Uint,
            },
            Box::new(|_, _, state: &State, _| Box::pin(async move { Ok(Value::Uint(state.foo)) })),
        );

        let mut scope = ftable.scope();
        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, &state, ast).await.unwrap();

        assert_eq!(val, Value::Uint(1))
    }

    #[tokio::test]
    async fn math() {
        let val = parse_and_eval("1 + 2", &()).await.unwrap();
        assert_eq!(val, Value::Uint(3));
    }

    #[tokio::test]
    async fn negate() {
        let val = parse_and_eval("-42", &()).await.unwrap();
        assert_eq!(val, Value::Int(-42));
    }

    #[tokio::test]
    async fn cast_float() {
        let val = parse_and_eval("(float (negate 42))", &()).await.unwrap();
        assert_eq!(val, Value::Float(-42.));
    }

    #[tokio::test]
    async fn cast_int() {
        let val = parse_and_eval("(int 4.2)", &()).await.unwrap();
        assert_eq!(val, Value::Int(4));
    }

    #[tokio::test]
    async fn cast_string() {
        let val = parse_and_eval("(str 4.2)", &()).await.unwrap();
        assert_eq!(val, Value::String("4.2".into()));
    }

    #[tokio::test]
    async fn math_with_precedence() {
        let val = parse_and_eval("1 + 2 * 3", &()).await.unwrap();
        assert_eq!(val, Value::Uint(7));
    }

    #[tokio::test]
    async fn lambda_1() {
        let val = parse_and_eval("(\\x -> x) 1", &()).await.unwrap();
        assert_eq!(val, Value::Uint(1));
    }

    #[tokio::test]
    async fn lambda_2() {
        let val = parse_and_eval("(\\x y -> x + y) 1 2", &()).await.unwrap();
        assert_eq!(val, Value::Uint(3));
    }

    #[tokio::test]
    async fn lambda_3() {
        let val = parse_and_eval("(\\x y z -> x + y * z) 1 2 3", &())
            .await
            .unwrap();
        assert_eq!(val, Value::Uint(7));
    }

    #[tokio::test]
    async fn let_in() {
        let val = parse_and_eval("let x = 1 + 2, y = 3 in x * y", &())
            .await
            .unwrap();
        assert_eq!(val, Value::Uint(9));
    }

    #[tokio::test]
    async fn len() {
        let val = parse_and_eval("len [1, 2, 3, 4]", &()).await.unwrap();
        assert_eq!(val, Value::Uint(4));
    }

    #[tokio::test]
    async fn len_tuple() {
        let val = parse_and_eval("len ((1, 2, 3, 4))", &()).await.unwrap();
        assert_eq!(val, Value::Uint(4));
    }

    #[tokio::test]
    async fn len_str() {
        let val = parse_and_eval("len 'asd'", &()).await.unwrap();
        assert_eq!(val, Value::Uint(3));
    }

    #[tokio::test]
    async fn has() {
        let val = parse_and_eval("has 'foo' ({ foo = 1 })", &())
            .await
            .unwrap();
        assert_eq!(val, Value::Bool(true));
    }

    #[tokio::test]
    async fn if_then_else_max() {
        let val = parse_and_eval("(\\x y -> if x > y then x else y) 4 20", &())
            .await
            .unwrap();
        assert_eq!(val, Value::Uint(20));
    }

    #[tokio::test]
    async fn if_then_else_max_with_comment() {
        let val = parse_and_eval("(\\ {- this is the max function -} x y -> if {- check which is bigger -} x > y then x {- x bigger -} else y {- y bigger -} ) 4 20", &()).await.unwrap();
        assert_eq!(val, Value::Uint(20));
    }

    #[tokio::test]
    async fn test_map() {
        let val = parse_and_eval("map (\\x -> x * 3 + 2 + 1) [1, 2, 3, 4]", &())
            .await
            .unwrap();
        assert_eq!(
            val,
            Value::List(vec![
                Value::Uint(6),
                Value::Uint(9),
                Value::Uint(12),
                Value::Uint(15)
            ])
        );
    }

    #[tokio::test]
    async fn test_map_result() {
        let val = parse_and_eval(
            r#"
            map
                (\x -> map (\y -> "*" ++ y) x)
                [ok "one", err 2, ok "three", err 4]
            "#,
            &(),
        )
        .await
        .unwrap();

        assert_eq!(
            val,
            Value::List(vec![
                Value::Result(Ok(Box::new(Value::String("*one".to_string())))),
                Value::Result(Err(Box::new(Value::Uint(2)))),
                Value::Result(Ok(Box::new(Value::String("*three".to_string())))),
                Value::Result(Err(Box::new(Value::Uint(4)))),
            ])
        );
    }

    #[tokio::test]
    async fn test_filter_map() {
        let val = parse_and_eval(
            r#"
            let
                results = [ok "one", err 2, ok "three", err 4],
                result_to_option = (λx → unwrap_or_else (λy → none) (map some x)),
                only_successful_results = λr → filter_map result_to_option r
            in
                only_successful_results results
            "#,
            &(),
        )
        .await
        .unwrap();

        assert_eq!(
            val,
            Value::List(vec![
                Value::String("one".to_string()),
                Value::String("three".to_string()),
            ])
        );

        let val = parse_and_eval(
            r#"
            let
                results = [ok "one", err 2, ok "three", err 4],
                only_successful_results = λr →
                    filter_map (unwrap_or_else (λy → none)) (map (λx → map some x) r),
            in
                only_successful_results results
            "#,
            &(),
        )
        .await
        .unwrap();

        assert_eq!(
            val,
            Value::List(vec![
                Value::String("one".to_string()),
                Value::String("three".to_string()),
            ])
        );
    }

    #[tokio::test]
    async fn test_unwrap_or_else() {
        let val = parse_and_eval(
            r#"
            map
                (\x -> unwrap_or_else (\y -> "TEST") x)
                [ok "one", err 2, ok "three", err 4],
            "#,
            &(),
        )
        .await
        .unwrap();

        assert_eq!(
            val,
            Value::List(vec![
                Value::String("one".to_string()),
                Value::String("TEST".to_string()),
                Value::String("three".to_string()),
                Value::String("TEST".to_string()),
            ])
        );
    }

    // FIXME: returns a closure, not a value
    // #[tokio::test]
    // async fn test_fold() {
    //     let val = parse_and_eval("(fold (\\x -> (\\y -> x + y)) 0 [1, 2, 3, 4])", &())
    //         .await
    //         .unwrap();
    //     assert_eq!(val, Value::Uint(10))
    // }

    #[tokio::test]
    async fn test_flatmap() {
        let val = parse_and_eval("flatmap ([[1, 2], [3, 4]])", &())
            .await
            .unwrap();
        assert_eq!(
            val,
            Value::List(vec![
                Value::Uint(1),
                Value::Uint(2),
                Value::Uint(3),
                Value::Uint(4)
            ])
        )
    }

    #[tokio::test]
    async fn test_zip() {
        let val = parse_and_eval("zip [1, 2, 3, 4] [4, 3, 2, 1]", &())
            .await
            .unwrap();

        assert_eq!(
            val,
            Value::List(vec![
                Value::Tuple(vec![Value::Uint(1), Value::Uint(4)]),
                Value::Tuple(vec![Value::Uint(2), Value::Uint(3)]),
                Value::Tuple(vec![Value::Uint(3), Value::Uint(2)]),
                Value::Tuple(vec![Value::Uint(4), Value::Uint(1)]),
            ])
        );
    }

    #[tokio::test]
    async fn test_filter() {
        let val = parse_and_eval("filter ((<) 2) [4, 3, 2, 1]", &())
            .await
            .unwrap();
        assert_eq!(val, Value::List(vec![Value::Uint(4), Value::Uint(3)]));
    }

    // test avg
    #[tokio::test]
    async fn test_avg() {
        let val = parse_and_eval("avg [1, 2, 3, 4]", &()).await.unwrap();
        assert_eq!(val, Value::Float(2.5))
    }

    // test sum
    #[tokio::test]
    async fn test_sum() {
        let val = parse_and_eval("sum [1, 2, 3, 4]", &()).await.unwrap();
        assert_eq!(val, Value::Uint(10))
    }

    // test min
    #[tokio::test]
    async fn test_min() {
        let val = parse_and_eval("list_min [4,3,2,5]", &()).await.unwrap();
        assert_eq!(val, Value::Uint(2))
    }

    // test take
    #[tokio::test]
    async fn test_take() {
        let val = parse_and_eval("take 2 [4 ,3, 2, 5]", &()).await.unwrap();
        assert_eq!(val, Value::List(vec![Value::Uint(4), Value::Uint(3)]))
    }

    // test skip
    #[tokio::test]
    async fn test_skip() {
        let val = parse_and_eval("skip 2 [4 ,3, 2, 5]", &()).await.unwrap();
        assert_eq!(val, Value::List(vec![Value::Uint(2), Value::Uint(5)]))
    }

    async fn parse_and_eval<S: Send + Sync + 'static>(
        code: &str,
        state: &S,
    ) -> Result<Value, crate::Error> {
        let mut parser = Parser::new(Token::tokenize(code).unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = Ftable::with_intrinsics(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        eval(&ctx, &ftable, state, ast).await
    }

    #[tokio::test]
    async fn test_ctor() {
        let tokens = Token::tokenize(
            "filter (λp → let a = x p, b = y p in a + b == 1)
 [Point2D { x = 0, y = 0 },
  Point2D { x = 0, y = 1 },
  Point3D { x = 1, y = 0, z = 2 },
  Point3D { x = 1, y = 1, z = 2 },
  PointI3D { i = none, x = 1, y = 0, z = 2 }
]",
        )
        .unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = ftable_with_point_adt(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

        assert_eq!(
            val,
            Value::List(vec![
                Value::Data(Data {
                    name: "Point2D".to_string(),
                    fields: Some(DataFields::Named(NamedDataFields {
                        fields: vec![
                            ("x".to_string(), Value::Uint(0)),
                            ("y".to_string(), Value::Uint(1))
                        ]
                        .into_iter()
                        .collect()
                    }))
                }),
                Value::Data(Data {
                    name: "Point3D".to_string(),
                    fields: Some(DataFields::Named(NamedDataFields {
                        fields: vec![
                            ("x".to_string(), Value::Uint(1)),
                            ("y".to_string(), Value::Uint(0)),
                            ("z".to_string(), Value::Uint(2))
                        ]
                        .into_iter()
                        .collect()
                    }))
                }),
                Value::Data(Data {
                    name: "PointI3D".to_string(),
                    fields: Some(DataFields::Named(NamedDataFields {
                        fields: vec![
                            ("i".to_string(), Value::Option(None)),
                            ("x".to_string(), Value::Uint(1)),
                            ("y".to_string(), Value::Uint(0)),
                            ("z".to_string(), Value::Uint(2))
                        ]
                        .into_iter()
                        .collect()
                    }))
                }),
            ])
        );
    }

    #[tokio::test]
    async fn test_ctor_with_defaults() {
        let tokens = Token::tokenize("filter (λp → let a = x p, b = y p in a + b == 1) [Point2D { x = 0, y = 0 }, Point2D { x = 0 }, Point2D {}, Point3D { x = let a = 1, b = 1 in a * b, y = 0, z = let a = 1, b = 1 in a + b }, Point3D { x = 1, y = 1, z = 2 }]").unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = ftable_with_point_adt(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, &(), ast).await.unwrap();

        assert_eq!(
            val,
            Value::List(vec![
                Value::Data(Data {
                    name: "Point2D".to_string(),
                    fields: Some(DataFields::Named(NamedDataFields {
                        fields: vec![
                            ("x".to_string(), Value::Uint(0)),
                            ("y".to_string(), Value::Uint(1))
                        ]
                        .into_iter()
                        .collect()
                    }))
                }),
                Value::Data(Data {
                    name: "Point3D".to_string(),
                    fields: Some(DataFields::Named(NamedDataFields {
                        fields: vec![
                            ("x".to_string(), Value::Uint(1)),
                            ("y".to_string(), Value::Uint(0)),
                            ("z".to_string(), Value::Uint(2))
                        ]
                        .into_iter()
                        .collect()
                    }))
                }),
            ])
        );
    }

    #[tokio::test]
    async fn test_10k_ctor_with_defaults() {
        // create an expression that builds 1000 points, maps them, and then filters them
        let mut expr =
            "filter (λp → let a = x p, b = y p in a + b == 1) (map (λi → i)  [".to_string();
        for i in 0..10_000 {
            expr += &format!("Point2D {{ x = {}, y = {} }}", i % 10, i / 10 % 10);
            if i + 1 < 10_000 {
                expr += ", ";
            }
        }

        expr += "])";

        let mut parser = Parser::new(Token::tokenize(&expr).unwrap());
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = ftable_with_point_adt(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, &(), ast).await;
        match val {
            Ok(val) => {
                if let Value::List(val) = val {
                    assert_eq!(val.len(), 200);
                } else {
                    panic!("expected Value::List, got {:?}", val);
                }
            }
            Err(e) => {
                eprintln!("{}", e);
                panic!("{}", sprint_trace(e.trace()));
            }
        }
    }

    #[tokio::test]
    async fn test_bad_ctor() {
        let tokens = Token::tokenize("filter (λp → let a = x p, b = y p in a + b + true == 1) [Point2D { x = 0, y = 0 }, Point2D { x = 0, y = 1 }, Point3D { x = 1, y = 0, z = 2 }, Point3D { x = 1, y = 1, z = 2 }]").unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();

        let mut id_dispenser = parser.id_dispenser;
        let ftable = ftable_with_point_adt(&mut id_dispenser);

        let mut scope = ftable.scope();

        let ctx = Context::new();
        let ast = resolve(&mut id_dispenser, &mut scope, expr).unwrap();
        let val = eval(&ctx, &ftable, &(), ast).await;

        match val {
            Ok(_) => unreachable!(),
            Err(e) => {
                assert_eq!(format!("{}", e), "expected uint, got `true`");
                assert_eq!(
                    sprint_trace(e.trace()),
                    r#"filter (λp → let a = x p in let b = y p in (==) ((+) ((+) a b) true) 1) [Point2D {x = 0, y = 0}, Point2D {x = 0, y = 1}, Point3D {x = 1, y = 0, z = 2}, Point3D {x = 1, y = 1, z = 2}]
  let a = x p in let b = y p in (==) ((+) ((+) a b) true) 1
    let b = y p in (==) ((+) ((+) a b) true) 1
      (==) ((+) ((+) a b) true) 1
        (==) ((+) ((+) a b) true)
          (+) ((+) a b) true
"#
                );
            }
        }
    }
}
