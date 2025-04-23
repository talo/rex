use rex::{Builder, Error, Program, Rex, Span, ToType, TypeError};
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
        Err(Error::TypeInference(TypeError::Other(
            Span::new(3, 21, 3, 66),
            "Missing keys: [\"c\", \"d\"]".to_string()
        )))
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
        Err(Error::TypeInference(TypeError::Other(
            Span::new(3, 21, 3, 35),
            "Missing keys: [\"b\"]".to_string()
        )))
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
        Err(Error::TypeInference(TypeError::UnboundVariable(
            Span::new(6, 17, 6, 20),
            "foo".to_string(),
        )))
    );
}
