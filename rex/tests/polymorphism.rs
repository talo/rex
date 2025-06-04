use rex_ast::{assert_expr_eq, b, d, expr::Expr, f, l, n, s, tup, u};
use rex_engine::{
    codec::{Func, Promise},
    engine::Builder,
    error::Error,
    ftable::{A, B},
    program::Program,
};
use rex_lexer::span::Span;
use rex_proc_macro::Rex;
use rex_type_system::{
    bool, float, list, string, tuple,
    types::{ToType, Type},
    uint,
};
use std::sync::Arc;
use uuid::Uuid;

#[tokio::test]
async fn test_function_overload_param_count_mismatch() {
    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    let res = builder.register_fn_core_with_name(
        "map",
        Type::build_arrow(vec![uint!(), uint!(), uint!()], uint!()),
        Box::new(move |_ctx, _args| {
            Box::pin(async move { Ok(Arc::new(Expr::Uint(Span::default(), 0))) })
        }),
    );

    assert_eq!(
        res,
        Err(Error::OverloadParamCountMismatch {
            name: "map".to_string(),
            new: 3,
            existing: 2,
            trace: Default::default(),
        })
    );
}

#[tokio::test]
async fn test_accessor_overload_param_count_mismatch() {
    #[derive(Rex)]
    struct Foo {
        a: u64,
        map: String,
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    let r = builder.register_adt(&Arc::new(Foo::to_type()), None, None);
    assert_eq!(
        r,
        Err(Error::OverloadParamCountMismatch {
            name: "map".to_string(),
            new: 1,
            existing: 2,
            trace: Default::default(),
        })
    );
}

#[tokio::test]
async fn test_field_accessors() {
    #[derive(Rex)]
    struct One {
        a: u64,
        b: String,
    }

    #[derive(Rex)]
    struct Two {
        a: bool,
        b: f64,
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Arc::new(One::to_type()), None, None)
        .unwrap();
    builder
        .register_adt(&Arc::new(Two::to_type()), None, None)
        .unwrap();
    let program = Program::compile(
        builder,
        r#"
        let
            one = One { a = 42, b = 'Hello' },
            two = Two { a = true, b = 2.5 }
        in
            (a one, b one, a two, b two)
        "#,
    )
    .unwrap();
    assert_eq!(
        program.res_type,
        tuple!(uint!(), string!(), bool!(), float!(),)
    );
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, tup!(u!(42), s!("Hello"), b!(true), f!(2.5)); ignore span);
}

#[tokio::test]
async fn test_map_adt() {
    #[derive(Rex)]
    struct Foo {
        a: u64,
        b: String,
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(
        builder,
        r#"
        map (λx → Foo { a = x, b = 'Hello' }) (list_range 1 4 None)
        "#,
    )
    .unwrap();
    assert_eq!(program.res_type, list!(Arc::new(Foo::to_type())));
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(
        res,
        l!(n!("Foo", Some(d!( a = u!(1), b = s!("Hello")))),
           n!("Foo", Some(d!( a = u!(2), b = s!("Hello")))),
           n!("Foo", Some(d!( a = u!(3), b = s!("Hello")))));
        ignore span);
}

#[tokio::test]
async fn test_promise_overload() {
    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder.register_fn1("produce_promise", |_ctx, uuid: Uuid| {
        let promise: Promise<u64> = Promise::new(uuid);
        Ok(promise)
    });

    builder.register_fn1("consume_promise", |_ctx, _promise: Promise<String>| {
        Ok(true)
    });

    builder.register_fn_async2("map", |_ctx, _f: Func<A, B>, x: Promise<A>| {
        Box::pin(async move {
            let res: Promise<B> = Promise::new(x.uuid);
            Ok(res)
        })
    });

    let program = Program::compile(
        builder,
        r#"
            (
                map (λx → x + 1) [1, 2, 3],
                consume_promise (map string (produce_promise random_uuid)),
            )

        "#,
    )
    .unwrap();
    assert_eq!(program.res_type, tuple!(list!(uint!()), bool!()));
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, tup!(l!(u!(2), u!(3), u!(4)), b!(true)); ignore span);
}
