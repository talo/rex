use rex::{
    type_system::{bool, float, list, string, uint},
    Builder, Error, Program, Rex, Span, ToType, Trace, TypeError,
};
use std::sync::Arc;

#[tokio::test]
async fn test_missing_fields() {
    #[derive(Rex)]
    pub struct Foo {
        pub a: u64,
        pub b: String,
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let res = Program::compile(
        builder,
        r#"
        let
            value = Foo { a = 42, b = "Hello", c = 99, d = true }
        in
            (a value, b value)
        "#,
    )
    .map(|_| ());
    assert_eq!(
        res,
        Err(Error::TypeInference {
            errors: vec![TypeError::Other(
                Span::new(3, 21, 3, 66),
                "Missing keys: [\"c\", \"d\"]".to_string()
            )],
            trace: Default::default(),
        })
    );

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let res = Program::compile(
        builder,
        r#"
        let
            value = Foo { a = 42 }
        in
            (a value, b value)
        "#,
    )
    .map(|_| ());
    assert_eq!(
        res,
        Err(Error::TypeInference {
            errors: vec![TypeError::Other(
                Span::new(3, 21, 3, 35),
                "Missing keys: [\"b\"]".to_string()
            )],
            trace: Default::default(),
        })
    );
}

#[tokio::test]
async fn test_unbound_variable() {
    let builder: Builder<()> = Builder::with_prelude().unwrap();
    let res = Program::compile(
        builder,
        r#"
        let
            a = 1,
            b = 2
        in
            (a, foo, b, bar)
        "#,
    )
    .map(|_| ());

    assert_eq!(
        res,
        Err(Error::TypeInference {
            errors: vec![
                TypeError::UnboundVariable(Span::new(6, 17, 6, 20), "foo".to_string(),),
                TypeError::UnboundVariable(Span::new(6, 25, 6, 28), "bar".to_string(),),
            ],
            trace: Default::default(),
        })
    );
}

#[tokio::test]
async fn test_multiple_unification_errors_tuple() {
    let builder: Builder<()> = Builder::with_prelude().unwrap();
    let res = Program::compile(
        builder,
        r#"
        ((sqrt "one"), (sqrt true), (sqrt [1]))
        "#,
    )
    .map(|_| ());
    assert_eq!(
        res,
        Err(Error::TypeInference {
            errors: vec![
                TypeError::CannotUnify(Span::new(2, 10, 2, 22), float!(), string!()),
                TypeError::CannotUnify(Span::new(2, 24, 2, 35), float!(), bool!()),
                TypeError::CannotUnify(Span::new(2, 37, 2, 47), float!(), list!(uint!())),
            ],
            trace: Default::default(),
        })
    );
}

#[tokio::test]
async fn test_multiple_unification_errors_let() {
    let builder: Builder<()> = Builder::with_prelude().unwrap();
    let res = Program::compile(
        builder,
        r#"
        let
            a = (sqrt "one"),
            b = (sqrt true),
            c = (sqrt [1]),
        in
            1
        "#,
    )
    .map(|_| ());
    assert_eq!(
        res,
        Err(Error::TypeInference {
            errors: vec![
                TypeError::CannotUnify(Span::new(3, 17, 3, 29), float!(), string!()),
                TypeError::CannotUnify(Span::new(4, 17, 4, 28), float!(), bool!()),
                TypeError::CannotUnify(Span::new(5, 17, 5, 27), float!(), list!(uint!())),
            ],
            trace: Default::default(),
        })
    );
}

#[tokio::test]
async fn test_incompatible_candidates() {
    let builder: Builder<()> = Builder::with_prelude().unwrap();
    let res = Program::compile(builder, r#"len true"#).map(|_| ());
    match &res {
        Err(Error::TypeInference { errors, .. }) => {
            assert_eq!(errors.len(), 1);
            assert!(matches!(
                errors[0],
                TypeError::IncompatibleCandidates(_, _, _)
            ));
        }
        _ => {
            panic!("Expected a type inference error");
        }
    }
}

#[tokio::test]
async fn test_runtime_error_direct() {
    let builder: Builder<()> = Builder::with_prelude().unwrap();
    let program = Program::compile(
        builder,
        r#"
        let
            a = \x -> len (list_range 0 10 (Some x)),
            b = \x -> (a x),
            c = \x -> (b x),
            d = \x -> (c x),
        in
            d 0
        "#,
    )
    .unwrap();
    let res = program.run(()).await;
    assert_eq!(
        res,
        Err(Error::Custom {
            error: "Step cannot be zero".to_string(),
            trace: Trace(vec![
                Span::new(3, 27, 3, 53),
                Span::new(3, 23, 3, 53),
                Span::new(4, 23, 4, 28),
                Span::new(5, 23, 5, 28),
                Span::new(6, 23, 6, 28),
                Span::new(8, 13, 8, 16),
            ])
        })
    );
}

#[tokio::test]
async fn test_runtime_error_nested() {
    let builder: Builder<()> = Builder::with_prelude().unwrap();
    let program = Program::compile(
        builder,
        r#"
        let
            a = \x -> len (list_range 0 10 (Some x)),
            b = \x -> (a x),
            c = \x -> map b [x],
            d = \x -> (c x),
        in
            d 0
        "#,
    )
    .unwrap();
    let res = program.run(()).await;
    assert_eq!(
        res,
        Err(Error::Custom {
            error: "Step cannot be zero".to_string(),
            trace: Trace(vec![
                Span::new(3, 27, 3, 53),
                Span::new(3, 23, 3, 53),
                Span::new(4, 23, 4, 28),
                Span::new(5, 23, 5, 32),
                Span::new(6, 23, 6, 28),
                Span::new(8, 13, 8, 16),
            ])
        })
    );
}
