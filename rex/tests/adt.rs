use chrono::{DateTime, Utc};
use rex::type_system::{adt, adt_variant, bool, float, int, list, string, tuple, uint};
use rex::{
    ast::{assert_expr_eq, b, d, expr::Expr, f, i, id::Id, l, n, s, tup, u},
    engine::codec::{Decode, Encode},
    json::{expr_to_json, json_to_expr},
    lexer::span::Span,
    type_system::types::{ToType, Type, ADT},
    Rex,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use uuid::{uuid, Uuid};

#[test]
fn test_struct() {
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
}

#[test]
fn test_struct_unit() {
    #[derive(Rex, Serialize, Deserialize, Clone, Debug, PartialEq)]
    struct Foo;

    let expected_type = Arc::new(Type::ADT(ADT {
        name: "Foo".to_string(),
        docs: None,
        variants: vec![],
    }));

    let expected_encoding = n!("Foo", None);

    compare(Foo, &expected_type, &expected_encoding);
}

#[test]
fn test_struct_unnamed_fields() {
    #[derive(Rex, Serialize, Deserialize, Clone, Debug, PartialEq)]
    struct Foo(u64, String);

    let expected_type = Arc::new(Type::ADT(adt! { Foo = Foo ( uint!(), string!() )}));

    assert_eq!(Foo::to_type(), *expected_type);

    let value = Foo(42, "Hello".to_string());
    let expected_encoding = n!("Foo", Some(tup!(u!(42), s!("Hello"))));

    compare(value, &expected_type, &expected_encoding);
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

#[test]
fn test_field_vec() {
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
}

#[test]
fn test_field_tuple() {
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
}

#[test]
fn test_field_optional() {
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
            b: Arc::new(Type::Option(uint!())),
            c: Arc::new(Type::Option(uint!())),
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

    compare(value, &expected_type, &expected_encoding);
}

#[test]
fn test_field_result() {
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
            i1: Arc::new(Type::Result(uint!(), string!())),
            i2: Arc::new(Type::Result(uint!(), string!())),
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
        Foo = Foo { a: string!(), b: Arc::new(Type::DateTime) }
    }));

    let expected_encoding = n!(
        "Foo",
        Some(d!(
            a = s!("Hello"),
            b = Expr::DateTime(
                Id::new(),
                Span::default(),
                DateTime::parse_from_rfc3339("2014-11-28T21:00:09+09:00")
                    .unwrap()
                    .into()
            ),
        ))
    );

    compare(value, &expected_type, &expected_encoding);
}

#[test]
fn test_field_uuid() {
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
        Foo = Foo { a: string!(), b: Arc::new(Type::Uuid) }
    }));

    let expected_encoding = n!(
        "Foo",
        Some(d!(
            a = s!("Hello"),
            b = Expr::Uuid(
                Id::new(),
                Span::default(),
                uuid!("f5d62567-7a45-4637-bbfb-252f4162574f")
            )
        ))
    );

    compare(value, &expected_type, &expected_encoding);
}

#[test]
fn test_enum_unit() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum Foo {
        One,
        Two,
    }

    let expected_type = Arc::new(Type::ADT(adt! {
        Foo = One . | Two .
    }));

    let expected_encoding1 = n!("One", None);
    let expected_encoding2 = n!("Two", None);

    compare(Foo::One, &expected_type, &expected_encoding1);
    compare(Foo::Two, &expected_type, &expected_encoding2);
}

#[test]
fn test_enum_named_fields() {
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
}

#[test]
fn test_enum_unnamed_fields() {
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
}

#[test]
fn test_enum_mixed() {
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
}

fn compare<T>(orig_value: T, expected_type: &Arc<Type>, expected_encoding: &Expr)
where
    T: ToType + Encode + Decode + Serialize + Clone + PartialEq + std::fmt::Debug,
{
    assert_eq!(**expected_type, T::to_type());

    let actual_encoding = orig_value.clone().try_encode(Span::default()).unwrap();
    let decoded_value = T::try_decode(&actual_encoding).unwrap();
    assert_eq!(orig_value, decoded_value);

    assert_expr_eq!(expected_encoding, actual_encoding; ignore span);
    let json_expected = serde_json::to_value(orig_value.clone()).unwrap();
    let json_actual = expr_to_json(&actual_encoding, &Arc::new(T::to_type())).unwrap();
    assert_eq!(json_expected, json_actual);
    let json_encoding = json_to_expr(&json_expected, &Arc::new(T::to_type())).unwrap();
    assert_expr_eq!(actual_encoding, json_encoding);
}
