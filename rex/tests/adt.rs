use chrono::{DateTime, Utc};
use rex::{
    ast::{assert_expr_eq, b, d, expr::Expr, f, i, l, n, s, tup, u},
    engine::{
        codec::{Decode, Encode, Promise},
        engine::{fn2, Builder},
        ftable::{Namespace, A},
        program::Program,
    },
    json::{expr_to_json, json_to_expr},
    lexer::span::Span,
    type_system::{
        adt, adt_variant, bool, float, int, list, option, promise, result, string, tuple,
        types::{ADTVariant, ToType, Type, TypeCon, ADT},
        uint,
    },
    Rex,
};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::sync::Arc;
use uuid::{uuid, Uuid};

#[tokio::test]
async fn test_struct() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: u64,
        pub b: String,
    }

    let value = Foo {
        a: 42,
        b: "Hello".to_string(),
    };

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = Foo {
            a: uint!(),
            b: string!(),
        }
    }));

    let expected_encoding = n!("Foo", Some(d!(a = u!(42), b = s!("Hello"),)));

    compare(value, &expected_type, &expected_encoding);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"Foo { a = 42, b = "Hello" }"#).unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding; ignore span);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(
        builder,
        r#"
        let value = Foo { a = 42, b = "Hello" }
        in (a value, b value)
    "#,
    )
    .unwrap();
    assert_eq!(program.res_type, tuple!(uint!(), string!()));
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, tup!(u!(42), s!("Hello")); ignore span);
}

#[tokio::test]
async fn test_struct_unit() {
    #[derive(Rex, Serialize, Deserialize, Clone, Debug, PartialEq)]
    struct Foo;

    let expected_type = Arc::new(Type::ADT(ADT {
        name: "Foo".to_string(),
        docs: None,
        variants: vec![],
    }));

    let expected_encoding = n!("Foo", None);

    compare(Foo, &expected_type, &expected_encoding);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"Foo"#).unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding; ignore span);
}

#[tokio::test]
async fn test_struct_single_unnamed_field() {
    #[derive(Rex, Serialize, Deserialize, Clone, Debug, PartialEq)]
    struct Foo(String);

    let expected_type = Arc::new(Type::ADT(ADT {
        name: "Foo".to_string(),
        docs: None,
        variants: vec![ADTVariant {
            name: "Foo".to_string(),
            t: Some(string!()),
            docs: None,
            t_docs: None,
            discriminant: None,
        }],
    }));

    assert_eq!(Foo::to_type(), *expected_type);

    let value = Foo("Hello".to_string());
    let expected_encoding = n!("Foo", Some(s!("Hello")));

    compare(value, &expected_type, &expected_encoding);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"Foo "Hello""#).unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding; ignore span);
}

#[tokio::test]
async fn test_struct_unnamed_fields() {
    #[derive(Rex, Serialize, Deserialize, Clone, Debug, PartialEq)]
    struct Foo(u64, String);

    let expected_type = Arc::new(Type::ADT(adt! { Foo = Foo ( uint!(), string!() )}));

    assert_eq!(Foo::to_type(), *expected_type);

    let value = Foo(42, "Hello".to_string());
    let expected_encoding = n!("Foo", Some(tup!(u!(42), s!("Hello"))));

    compare(value, &expected_type, &expected_encoding);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"Foo 42 "Hello""#).unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding; ignore span);
}

#[test]
fn test_field_atomic() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        bt: bool,
        bf: bool,

        u8: u8,
        u16: u16,
        u32: u32,
        u64: u64,

        i8: i8,
        i16: i16,
        i32: i32,
        i64: i64,

        f32: f32,
        f64: f64,
        s: String,
    }

    let value = Foo {
        bt: true,
        bf: false,

        u8: 255,
        u16: 65535,
        u32: 4294967295,
        u64: 281474976710655,

        i8: -128,
        i16: -32768,
        i32: -2147483648,
        i64: -281474976710655,

        f32: 3.5,
        f64: 4.5,
        s: "Hello".to_string(),
    };

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = Foo {
            bt: bool!(),
            bf: bool!(),

            u8: uint!(),
            u16: uint!(),
            u32: uint!(),
            u64: uint!(),

            i8: int!(),
            i16: int!(),
            i32: int!(),
            i64: int!(),

            f32: float!(),
            f64: float!(),
            s: string!(),
        }
    }));

    let encoded = n!(
        "Foo",
        Some(d!(
            bt = b!(true),
            bf = b!(false),
            u8 = u!(255),
            u16 = u!(65535),
            u32 = u!(4294967295),
            u64 = u!(281474976710655),
            i8 = i!(-128),
            i16 = i!(-32768),
            i32 = i!(-2147483648),
            i64 = i!(-281474976710655),
            f32 = f!(3.5),
            f64 = f!(4.5),
            s = s!("Hello"),
        ))
    );

    compare(value, &expected_type, &encoded);
}

#[tokio::test]
async fn test_field_vec() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub b: Vec<u64>,
    }

    let value = Foo {
        a: "Hello".to_string(),
        b: vec![12, 345, 6789],
    };

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = Foo {
            a: string!(),
            b: list!(uint!()),
        }
    }));

    let expected_encoding = n!(
        "Foo",
        Some(d!(a = s!("Hello"), b = l!(u!(12), u!(345), u!(6789))))
    );

    compare(value, &expected_type, &expected_encoding);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(
        builder,
        r#"
            Foo {
                a = "Hello",
                b = [12, 345, 6789]
            }
        "#,
    )
    .unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding; ignore span);
}

#[tokio::test]
async fn test_field_tuple() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub b: (u64, bool, f64, String),
    }

    let value = Foo {
        a: "Hello".to_string(),
        b: (99, true, 3.14, "test".to_string()),
    };

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = Foo {
            a: string!(),
            b: tuple!(uint!(), bool!(), float!(), string!()),
        }
    }));

    let expected_encoding = n!(
        "Foo",
        Some(d!(
            a = s!("Hello"),
            b = tup!(u!(99), b!(true), f!(3.14), s!("test"))
        ))
    );

    compare(value, &expected_type, &expected_encoding);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(
        builder,
        r#"
            Foo {
                a = "Hello",
                b = (99, true, 3.14, "test")
            }
        "#,
    )
    .unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding; ignore span);
}

#[tokio::test]
async fn test_field_optional() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub b: Option<u64>,
        pub c: Option<u64>,
    }

    let value = Foo {
        a: "Hello".to_string(),
        b: Some(42),
        c: None,
    };

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = Foo {
            a: string!(),
            b: option!(uint!()),
            c: option!(uint!()),
        }
    }));

    let expected_encoding = n!(
        "Foo",
        Some(d!(
            a = s!("Hello"),
            b = n!("Some", Some(u!(42))),
            c = n!("None", None)
        ))
    );

    compare(value.clone(), &expected_type, &expected_encoding);
    compare_from_json(
        value.clone(),
        &expected_encoding,
        "{\"a\":\"Hello\",\"b\":42,\"c\":null}",
    );
    compare_from_json(
        value.clone(),
        &expected_encoding,
        "{\"a\":\"Hello\",\"b\":42,\"c\":null,\"x\":4}",
    );
    compare_from_json(
        value.clone(),
        &expected_encoding,
        "{\"a\":\"Hello\",\"b\":42}",
    );
    compare_from_json(
        value.clone(),
        &expected_encoding,
        "{\"a\":\"Hello\",\"b\":42,\"x\":4}",
    );

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(
        builder,
        r#"
            Foo {
                a = "Hello",
                b = Some 42,
                c = None,
            }
        "#,
    )
    .unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding; ignore span);
}

#[tokio::test]
async fn test_field_result() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub i1: Result<u64, String>,
        pub i2: Result<u64, String>,
    }

    let value = Foo {
        a: "Hello".to_string(),
        i1: Ok(123),
        i2: Err("bad".to_string()),
    };

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = Foo {
            a: string!(),
            i1: result!(string!(), uint!()),
            i2: result!(string!(), uint!()),
        }
    }));

    let expected_encoding = n!(
        "Foo",
        Some(d!(
            a = s!("Hello"),
            i1 = n!("Ok", Some(u!(123))),
            i2 = n!("Err", Some(s!("bad"))),
        ))
    );

    compare(value, &expected_type, &expected_encoding);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(
        builder,
        r#"
            Foo {
                a = "Hello",
                i1 = Ok 123,
                i2 = Err "bad",
            }
        "#,
    )
    .unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding; ignore span);
}

#[tokio::test]
async fn test_field_promise() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub i1: Promise<String>,
        pub i2: Promise<u64>,
    }

    let value = Foo {
        a: "Hello".to_string(),
        i1: Promise::new(uuid!("00000000-0000-0000-0000-000000000001")),
        i2: Promise::new(uuid!("00000000-0000-0000-0000-000000000002")),
    };

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = Foo {
            a: string!(),
            i1: promise!(string!()),
            i2: promise!(uint!()),
        }
    }));

    let expected_encoding = n!(
        "Foo",
        Some(d!(
            a = s!("Hello"),
            i1 = Arc::new(Expr::Promise(
                Span::default(),
                uuid!("00000000-0000-0000-0000-000000000001")
            )),
            i2 = Arc::new(Expr::Promise(
                Span::default(),
                uuid!("00000000-0000-0000-0000-000000000002")
            )),
        ))
    );

    compare(value, &expected_type, &expected_encoding);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    builder.register(
        &Namespace::test(),
        "make_promise",
        fn2(|_ctx, uuid: Uuid, _value: A| {
            let promise: Promise<A> = Promise::new(uuid);
            Ok(promise)
        }),
    );
    let program = Program::compile(
        builder,
        r#"
            Foo {
                a = "Hello",
                i1 = make_promise (uuid "00000000-0000-0000-0000-000000000001") "hello",
                i2 = make_promise (uuid "00000000-0000-0000-0000-000000000002") 123,
            }
        "#,
    )
    .unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding; ignore span);
}

#[test]
fn test_field_datetime() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub b: DateTime<Utc>,
    }

    let value = Foo {
        a: "Hello".to_string(),
        b: DateTime::parse_from_rfc3339("2014-11-28T21:00:09+09:00")
            .unwrap()
            .into(),
    };

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = Foo { a: string!(), b: Arc::new(Type::Con(TypeCon::DateTime)) }
    }));

    let expected_encoding = n!(
        "Foo",
        Some(d!(
            a = s!("Hello"),
            b = Arc::new(Expr::DateTime(
                Span::default(),
                DateTime::parse_from_rfc3339("2014-11-28T21:00:09+09:00")
                    .unwrap()
                    .into()
            )),
        ))
    );

    compare(value, &expected_type, &expected_encoding);
}

#[tokio::test]
async fn test_field_uuid() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub b: Uuid,
    }

    let value = Foo {
        a: "Hello".to_string(),
        b: uuid!("f5d62567-7a45-4637-bbfb-252f4162574f"),
    };

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = Foo { a: string!(), b: Arc::new(Type::Con(TypeCon::Uuid)) }
    }));

    let expected_encoding = n!(
        "Foo",
        Some(d!(
            a = s!("Hello"),
            b = Arc::new(Expr::Uuid(
                Span::default(),
                uuid!("f5d62567-7a45-4637-bbfb-252f4162574f")
            ))
        ))
    );

    compare(value, &expected_type, &expected_encoding);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(
        builder,
        r#"
            Foo {
                a = "Hello",
                b = uuid "f5d62567-7a45-4637-bbfb-252f4162574f",
            }
        "#,
    )
    .unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding; ignore span);
}

#[tokio::test]
async fn test_enum_unit() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum Color {
        Red,
        Green,
        Blue,
    }

    let expected_type = Arc::new(Type::ADT(adt! {
        Color = Red . | Green . | Blue .
    }));

    let expected_encoding1 = n!("Red", None);
    let expected_encoding2 = n!("Green", None);
    let expected_encoding3 = n!("Blue", None);

    compare(Color::Red, &expected_type, &expected_encoding1);
    compare(Color::Green, &expected_type, &expected_encoding2);
    compare(Color::Blue, &expected_type, &expected_encoding3);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Color::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"(Color::Red, Color::Green, Color::Blue)"#).unwrap();
    assert_eq!(
        program.res_type,
        tuple!(
            Arc::new(Color::to_type()),
            Arc::new(Color::to_type()),
            Arc::new(Color::to_type())
        )
    );
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(
        res,
        tup!(n!("Red", None), n!("Green", None), n!("Blue", None));
        ignore span);
}

#[tokio::test]
async fn test_enum_unit_int() {
    // This should be handled identically to the version without int values
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum Color {
        Red = 1,
        Green = 2,
        Blue = 3,
    }

    let expected_type = Arc::new(Type::ADT(ADT {
        docs: None,
        name: "Color".to_string(),
        variants: vec![
            ADTVariant {
                name: "Red".to_string(),
                t: None,
                docs: None,
                t_docs: None,
                discriminant: Some(1),
            },
            ADTVariant {
                name: "Green".to_string(),
                t: None,
                docs: None,
                t_docs: None,
                discriminant: Some(2),
            },
            ADTVariant {
                name: "Blue".to_string(),
                t: None,
                docs: None,
                t_docs: None,
                discriminant: Some(3),
            },
        ],
    }));

    let expected_encoding1 = n!("Red", None);
    let expected_encoding2 = n!("Green", None);
    let expected_encoding3 = n!("Blue", None);

    compare(Color::Red, &expected_type, &expected_encoding1);
    compare(Color::Green, &expected_type, &expected_encoding2);
    compare(Color::Blue, &expected_type, &expected_encoding3);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Color::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"(Color::Red, Color::Green, Color::Blue)"#).unwrap();
    assert_eq!(
        program.res_type,
        tuple!(
            Arc::new(Color::to_type()),
            Arc::new(Color::to_type()),
            Arc::new(Color::to_type())
        )
    );
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(
        res,
        tup!(n!("Red", None), n!("Green", None), n!("Blue", None));
        ignore span);
}

#[tokio::test]
async fn test_enum_named_fields() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum Foo {
        One { a: u64, b: String },
        Two { c: bool, d: f64 },
    }

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = One { a: uint!(), b: string!() }
            | Two { c: bool!(), d: float!() }
    }));

    let expected_encoding1 = n!("One", Some(d!(a = u!(42), b = s!("Hello"),)));

    let expected_encoding2 = n!("Two", Some(d!(c = b!(true), d = f!(2.5),)));

    let value1 = Foo::One {
        a: 42,
        b: "Hello".to_string(),
    };

    let value2 = Foo::Two { c: true, d: 2.5 };

    compare(value1, &expected_type, &expected_encoding1);
    compare(value2, &expected_type, &expected_encoding2);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"Foo::One { a = (21 * 2), b = 'Hello' }"#).unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding1; ignore span);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"Foo::Two { c = true, d = (5.0 / 2.0) }"#).unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding2; ignore span);
}

#[tokio::test]
async fn test_enum_unnamed_fields() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum Foo {
        One(u64, String),
        Two(bool, f64, u64),
    }

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = One ( uint!(), string!() )
            | Two ( bool!(), float!(), uint!() )
    }));

    let value1 = Foo::One(42, "Hello".to_string());
    let value2 = Foo::Two(true, 2.5, 99);
    let expected_encoding1 = n!("One", Some(tup!(u!(42), s!("Hello"))));
    let expected_encoding2 = n!("Two", Some(tup!(b!(true), f!(2.5), u!(99))));

    compare(value1, &expected_type, &expected_encoding1);
    compare(value2, &expected_type, &expected_encoding2);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"Foo::One (21 * 2) 'Hello'"#).unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding1; ignore span);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"Foo::Two true (5.0 / 2.0) (100 - 1)"#).unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding2; ignore span);
}

#[tokio::test]
async fn test_enum_rename() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum Color {
        Red,
        Green,
        #[serde(rename = "blOO")]
        Blue,
    }

    let expected_type = Arc::new(Type::ADT(adt! {
        Color = Red . | Green . | blOO .
    }));

    let expected_encoding1 = n!("Red", None);
    let expected_encoding2 = n!("Green", None);
    let expected_encoding3 = n!("blOO", None);

    compare(Color::Red, &expected_type, &expected_encoding1);
    compare(Color::Green, &expected_type, &expected_encoding2);
    compare(Color::Blue, &expected_type, &expected_encoding3);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Color::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"(Color::Red, Color::Green, Color::blOO)"#).unwrap();
    assert_eq!(
        program.res_type,
        tuple!(
            Arc::new(Color::to_type()),
            Arc::new(Color::to_type()),
            Arc::new(Color::to_type())
        )
    );
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(
        res,
        tup!(n!("Red", None), n!("Green", None), n!("blOO", None));
        ignore span);
}

#[tokio::test]
async fn test_enum_mixed() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum Foo {
        One,
        Two { a: u64, b: String },
        Three(bool, f64, u64),
    }

    let expected_type = Arc::new(Type::ADT(adt!(
        Foo = One .
            | Two { a: uint!(), b: string!() }
            | Three ( bool!(), float!(), uint!() )
    )));

    let value1 = Foo::One;
    let expected_encoding1 = n!("One", None);

    let value2 = Foo::Two {
        a: 42,
        b: "Hello".to_string(),
    };
    let expected_encoding2 = n!("Two", Some(d!(a = u!(42), b = s!("Hello"))));

    let value3 = Foo::Three(true, 2.5, 99);
    let expected_encoding3 = n!("Three", Some(tup!(b!(true), f!(2.5), u!(99))));

    compare(value1, &expected_type, &expected_encoding1);
    compare(value2, &expected_type, &expected_encoding2);
    compare(value3, &expected_type, &expected_encoding3);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Foo::to_type()), None, None)
        .unwrap();
    let program = Program::compile(
        builder,
        r#"
        [
            Foo::One,
            Foo::Two { a = 42, b = "Hello" },
            Foo::Three true (5.0 / 2.0) (100 - 1)
        ]
        "#,
    )
    .unwrap();
    assert_eq!(program.res_type, list!(expected_type));
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(
        res,
        l!(expected_encoding1, expected_encoding2, expected_encoding3);
        ignore span);
}

#[tokio::test]
async fn test_json_field() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub data: serde_json::Value,
    }

    let value = Foo {
        data: json!({ "a": 1, "b": 2 }),
    };

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = Foo {
            data: Arc::new(Type::ADT(ADT {
                name: "serde_json::Value".to_string(),
                docs: None,
                variants: vec![
                    ADTVariant {
                        name: "serde_json::Value".to_string(),
                        t: Some(string!()),
                        docs: None,
                        t_docs: None,
                        discriminant: None,
                    }
                ]
            }))
        }
    }));

    let expected_encoding = n!(
        "Foo",
        Some(d!(
            data = n!("serde_json::Value", Some(s!("{\"a\":1,\"b\":2}")))
        ))
    );

    compare(value, &expected_type, &expected_encoding);
}

#[tokio::test]
async fn test_derive_name() {
    mod first {
        use super::*;
        #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
        #[rex(name = "Foo")]
        pub struct Model {
            pub a: u64,
            pub b: String,
        }
    }
    mod second {
        use super::*;
        #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
        #[rex(name = "Bar")]
        pub struct Model(pub u64, pub bool);
    }
    mod third {
        use super::*;
        #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
        #[rex(name = "Color")]
        pub enum Model {
            Red,
            Green,
            Blue,
        }
    }

    let value = (
        first::Model {
            a: 42,
            b: "Hello".to_string(),
        },
        second::Model(55, true),
        third::Model::Blue,
    );

    let expected_type = Arc::new(Type::Tuple(vec![
        Arc::new(Type::ADT(adt! {
            Foo = Foo {
                a: uint!(),
                b: string!(),
            }
        })),
        Arc::new(Type::ADT(adt! {
            Bar = Bar ( uint!(), bool!() )
        })),
        Arc::new(Type::ADT(adt! {
            Color = Red . | Green . | Blue .
        })),
    ]));

    let expected_encoding = tup!(
        n!("Foo", Some(d!(a = u!(42), b = s!("Hello"),))),
        n!("Bar", Some(tup!(u!(55), b!(true)))),
        n!("Blue", None),
    );

    compare(value, &expected_type, &expected_encoding);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(
            &Namespace::rex(),
            &Arc::new(first::Model::to_type()),
            None,
            None,
        )
        .unwrap();
    builder
        .register_adt(
            &Namespace::rex(),
            &Arc::new(second::Model::to_type()),
            None,
            None,
        )
        .unwrap();
    builder
        .register_adt(
            &Namespace::rex(),
            &Arc::new(third::Model::to_type()),
            None,
            None,
        )
        .unwrap();
    let program = Program::compile(
        builder,
        r#"
        (
            Foo { a = 42, b = "Hello" },
            Bar 55 true,
            Color::Blue,
        )
        "#,
    )
    .unwrap();
    assert_eq!(program.res_type, expected_type);
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, expected_encoding; ignore span);
}

fn compare<T>(orig_value: T, expected_type: &Arc<Type>, expected_encoding: &Expr)
where
    T: ToType + Encode + Decode + Serialize + Clone + PartialEq + std::fmt::Debug,
{
    assert_eq!(**expected_type, T::to_type());

    let actual_encoding = orig_value.clone().try_encode(Span::default()).unwrap();
    assert_expr_eq!(expected_encoding, actual_encoding; ignore span);

    let decoded_value = T::try_decode(&actual_encoding).unwrap();
    assert_eq!(orig_value, decoded_value);

    let json_expected = serde_json::to_value(orig_value.clone()).unwrap();
    let json_actual = expr_to_json(
        &actual_encoding,
        &Arc::new(T::to_type()),
        &Default::default(),
    )
    .unwrap();
    assert_eq!(json_expected, json_actual);

    let json_encoding =
        json_to_expr(&json_expected, &Arc::new(T::to_type()), &Default::default()).unwrap();
    assert_expr_eq!(actual_encoding, json_encoding; ignore span);
}

fn compare_from_json<T>(orig_value: T, expected_encoding: &Expr, json: &str)
where
    T: ToType
        + Encode
        + Decode
        + Serialize
        + for<'a> Deserialize<'a>
        + Clone
        + PartialEq
        + std::fmt::Debug,
{
    let deserialized_value = serde_json::from_str::<T>(json).unwrap();
    assert_eq!(deserialized_value, orig_value);

    let deserialized_json = serde_json::from_str::<serde_json::Value>(json).unwrap();
    let actual_encoding = json_to_expr(
        &deserialized_json,
        &Arc::new(T::to_type()),
        &Default::default(),
    )
    .unwrap();
    assert_expr_eq!(actual_encoding, expected_encoding; ignore span);
}

#[tokio::test]
async fn test_adt_equality() {
    // Test enum equality
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum Color {
        Red,
        Green,
        Blue,
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Color::to_type()), None, None)
        .unwrap();
    
    // Test equality of same variant
    let program = Program::compile(builder, r#"Color::Red == Color::Red"#).unwrap();
    assert_eq!(program.res_type, bool!());
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, b!(true); ignore span);

    // Test inequality of different variants
    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Color::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"Color::Red == Color::Blue"#).unwrap();
    assert_eq!(program.res_type, bool!());
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, b!(false); ignore span);

    // Test != operator
    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Color::to_type()), None, None)
        .unwrap();
    let program = Program::compile(builder, r#"Color::Red != Color::Blue"#).unwrap();
    assert_eq!(program.res_type, bool!());
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, b!(true); ignore span);

    // Test struct equality with fields
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Point {
        pub x: u64,
        pub y: u64,
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Point::to_type()), None, None)
        .unwrap();
    
    // Same values should be equal
    let program = Program::compile(
        builder,
        r#"Point { x = 10, y = 20 } == Point { x = 10, y = 20 }"#,
    )
    .unwrap();
    assert_eq!(program.res_type, bool!());
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, b!(true); ignore span);

    // Different values should not be equal
    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Point::to_type()), None, None)
        .unwrap();
    let program = Program::compile(
        builder,
        r#"Point { x = 10, y = 20 } == Point { x = 15, y = 20 }"#,
    )
    .unwrap();
    assert_eq!(program.res_type, bool!());
    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, b!(false); ignore span);

    // Test with filter - common use case
    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder
        .register_adt(&Namespace::rex(), &Arc::new(Color::to_type()), None, None)
        .unwrap();
    let program = Program::compile(
        builder,
        r#"filter (\c -> c == Color::Red) [Color::Red, Color::Blue, Color::Red, Color::Green]"#,
    )
    .unwrap();
    assert_eq!(program.res_type, list!(Arc::new(Color::to_type())));
    let res = program.run(()).await.unwrap();
    // Should return list with two Red values
    match &*res {
        Expr::List(_, items) => {
            assert_eq!(items.len(), 2);
        }
        _ => panic!("Expected a list"),
    }
}