use rex::{
    type_system::{bool, dict, float, list, string, uint},
    Builder, Error, Program, Rex, Span, ToType, Trace, Type, TypeError,
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
            errors: vec![TypeError::DictKeysMismatch(
                Span::new(3, 21, 3, 66),
                "".to_string(),
                vec!["c".to_string(), "d".to_string()]
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
            errors: vec![TypeError::DictKeysMismatch(
                Span::new(3, 21, 3, 35),
                "".to_string(),
                vec!["b".to_string()]
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
                TypeError::CannotUnify(
                    Span::new(2, 10, 2, 22),
                    "".to_string(),
                    float!(),
                    string!()
                ),
                TypeError::CannotUnify(Span::new(2, 24, 2, 35), "".to_string(), float!(), bool!()),
                TypeError::CannotUnify(
                    Span::new(2, 37, 2, 47),
                    "".to_string(),
                    float!(),
                    list!(uint!())
                ),
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
                TypeError::CannotUnify(
                    Span::new(3, 17, 3, 29),
                    "".to_string(),
                    float!(),
                    string!()
                ),
                TypeError::CannotUnify(Span::new(4, 17, 4, 28), "".to_string(), float!(), bool!()),
                TypeError::CannotUnify(
                    Span::new(5, 17, 5, 27),
                    "".to_string(),
                    float!(),
                    list!(uint!())
                ),
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

#[tokio::test]
async fn test_path_error1() {
    #[derive(Rex, Debug, PartialEq, Clone)]
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
        (
            Foo { a = 42, b = "Hello" },
            Foo { a = true, b = "Hello" },
            Foo { a = 42, b = true },
        )
    "#,
    )
    .map(|_| ());
    assert_eq!(
        res,
        Err(Error::TypeInference {
            errors: vec![
                TypeError::CannotUnify(
                    Span::new(4, 13, 4, 42),
                    "In property a".to_string(),
                    uint!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(5, 13, 5, 37),
                    "In property b".to_string(),
                    string!(),
                    bool!()
                ),
            ],
            trace: Default::default(),
        })
    );
}

#[tokio::test]
async fn test_path_error2() {
    #[derive(Rex, Debug, PartialEq, Clone)]
    pub struct Bar {
        pub a: ((String, bool), (bool, f64), ((f64, String), (u64, bool))),
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Arc::new(Bar::to_type()), None, None)
        .unwrap();
    let res = Program::compile(
        builder,
        r#"
        (
            Bar { a = (("Hello", true), (true, 3.5), ((3.5, "Hello"), (12, false))) },
            Bar { a = ((true, true), (true, 3.5), ((3.5, "Hello"), (12, false))) },
            Bar { a = (("Hello", 1), (true, 3.5), ((3.5, "Hello"), (12, false))) },
            Bar { a = (("Hello", true), (1, 3.5), ((3.5, "Hello"), (12, false))) },
            Bar { a = (("Hello", true), (true, true), ((3.5, "Hello"), (12, false))) },
            Bar { a = (("Hello", true), (true, 3.5), ((true, "Hello"), (12, false))) },
            Bar { a = (("Hello", true), (true, 3.5), ((3.5, true), (12, false))) },
            Bar { a = (("Hello", true), (true, 3.5), ((3.5, "Hello"), (true, false))) },
            Bar { a = (("Hello", true), (true, 3.5), ((3.5, "Hello"), (12, 1))) },
        )
    "#,
    )
    .map(|_| ());
    assert_eq!(
        res,
        Err(Error::TypeInference {
            errors: vec![
                TypeError::CannotUnify(
                    Span::new(4, 13, 4, 83),
                    "In property a[0][0]".to_string(),
                    string!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(5, 13, 5, 83),
                    "In property a[0][1]".to_string(),
                    bool!(),
                    uint!()
                ),
                TypeError::CannotUnify(
                    Span::new(6, 13, 6, 83),
                    "In property a[1][0]".to_string(),
                    bool!(),
                    uint!()
                ),
                TypeError::CannotUnify(
                    Span::new(7, 13, 7, 87),
                    "In property a[1][1]".to_string(),
                    float!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(8, 13, 8, 87),
                    "In property a[2][0][0]".to_string(),
                    float!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(9, 13, 9, 83),
                    "In property a[2][0][1]".to_string(),
                    string!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(10, 13, 10, 88),
                    "In property a[2][1][0]".to_string(),
                    uint!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(11, 13, 11, 82),
                    "In property a[2][1][1]".to_string(),
                    bool!(),
                    uint!()
                ),
            ],
            trace: Default::default(),
        })
    );
}

#[tokio::test]
async fn test_path_error3() {
    let dict3 = dict! { a: uint!(), b: string!() };
    let dict2 = dict! { three: dict3.clone() };
    let dict1 = dict! { two: dict2.clone() };
    let dict0 = dict! { one: dict1.clone() };

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_fn_core_with_name(
            "f",
            Arc::new(Type::make_arrow(dict0.clone(), dict0.clone())),
            Box::new(move |_, args| Box::pin(async move { Ok(args[0].clone()) })),
        )
        .unwrap();

    let res = Program::compile(
        builder,
        r#"
        (
            f { one = { two = { three = { a = 4, b = "Hello" } } } },
            f { one = { two = { three = { a = true, b = "Hello" } } } },
            f { one = { two = { three = { a = 4, b = true } } } },
            f { one = { two = { three = true } } },
            f { one = { two = true } },
            f { one = true },
            f true,
        )
    "#,
    )
    .map(|_| ());

    assert_eq!(
        res,
        Err(Error::TypeInference {
            errors: vec![
                TypeError::CannotUnify(
                    Span::new(4, 13, 4, 72),
                    "In property one.two.three.a".to_string(),
                    uint!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(5, 13, 5, 66),
                    "In property one.two.three.b".to_string(),
                    string!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(6, 13, 6, 51),
                    "In property one.two.three".to_string(),
                    dict3,
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(7, 13, 7, 39),
                    "In property one.two".to_string(),
                    dict2,
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(8, 13, 8, 29),
                    "In property one".to_string(),
                    dict1,
                    bool!()
                ),
                TypeError::CannotUnify(Span::new(9, 13, 9, 19), "".to_string(), dict0, bool!()),
            ],
            trace: Default::default(),
        })
    );
}

#[tokio::test]
async fn test_path_error_multiple() {
    #[derive(Rex, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: u64,
        pub b: String,
        pub c: bool,
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let res = Program::compile(
        builder,
        r#"
        (
            Foo { a = 42, b = "Hello", c = true },
            Foo { a = 42, b = true, c = true },
            Foo { a = true, b = true, c = true },
            Foo { a = true, b = true, c = 42 },
        )
    "#,
    )
    .map(|_| ());
    assert_eq!(
        res,
        Err(Error::TypeInference {
            errors: vec![
                TypeError::CannotUnify(
                    Span::new(4, 13, 4, 47),
                    "In property b".to_string(),
                    string!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(5, 13, 5, 49),
                    "In property a".to_string(),
                    uint!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(5, 13, 5, 49),
                    "In property b".to_string(),
                    string!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(6, 13, 6, 47),
                    "In property a".to_string(),
                    uint!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(6, 13, 6, 47),
                    "In property b".to_string(),
                    string!(),
                    bool!()
                ),
                TypeError::CannotUnify(
                    Span::new(6, 13, 6, 47),
                    "In property c".to_string(),
                    bool!(),
                    uint!()
                ),
            ],
            trace: Default::default(),
        })
    );
}
