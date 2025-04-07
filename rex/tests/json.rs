use chrono::{DateTime, Utc};
use rex::{
    ast::assert_expr_eq,
    engine::codec::Encode,
    json::{expr_to_json, json_to_expr},
    lexer::span::Span,
    type_system::types::ToType,
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

    let foo = Foo {
        a: 42,
        b: "Hello".to_string(),
    };

    compare(foo);
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

    let foo = Foo {
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

        f32: 3.1,
        f64: 4.2,
        s: "Hello".to_string(),
    };

    compare(foo);
}

#[test]
fn test_field_vec() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub b: Vec<u64>,
    }

    let foo = Foo {
        a: "Hello".to_string(),
        b: vec![12, 345, 6789],
    };

    compare(foo);
}

#[test]
fn test_field_tuple() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub b: (u64, bool, f64, String),
    }

    let foo = Foo {
        a: "Hello".to_string(),
        b: (99, true, 3.14, "test".to_string()),
    };

    compare(foo);
}

#[test]
fn test_field_optional() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub b: Option<u64>,
        pub c: Option<u64>,
    }

    let foo = Foo {
        a: "Hello".to_string(),
        b: Some(42),
        c: None,
    };

    compare(foo);
}

#[test]
fn test_field_result() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub i1: Result<u64, String>,
        pub i2: Result<u64, String>,
    }

    let foo = Foo {
        a: "Hello".to_string(),
        i1: Ok(123),
        i2: Err("bad".to_string()),
    };

    compare(foo);
}

#[test]
fn test_field_datetime() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub b: DateTime<Utc>,
    }

    let foo = Foo {
        a: "Hello".to_string(),
        b: DateTime::parse_from_rfc3339("2014-11-28T21:00:09+09:00")
            .unwrap()
            .into(),
    };

    compare(foo);
}

#[test]
fn test_field_uuid() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Foo {
        pub a: String,
        pub b: Uuid,
    }

    let foo = Foo {
        a: "Hello".to_string(),
        b: uuid!("f5d62567-7a45-4637-bbfb-252f4162574f"),
    };

    compare(foo);
}

#[test]
fn test_enum_unit() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum Foo {
        One,
        Two,
    }

    compare(Foo::One);
    compare(Foo::Two);
}

#[test]
fn test_enum_named_fields() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum Foo {
        One { a: u64, b: String },
        Two { c: bool, d: f64 },
    }

    compare(Foo::One {
        a: 42,
        b: "Hello".to_string(),
    });
    compare(Foo::Two { c: true, d: 2.5 });
}

#[test]
fn test_enum_unnamed_fields() {
    #[derive(Rex, Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum Foo {
        One(u64, String),
        Two(bool, f64, u64),
    }

    compare(Foo::One(42, "Hello".to_string()));
    compare(Foo::Two(true, 2.5, 99));
}

fn compare<T>(foo: T)
where
    T: ToType + Encode + Serialize + Clone,
{
    let expr1 = foo.clone().try_encode(Span::default()).unwrap();
    // println!("expr1 = {}", expr1);
    let json_expected = serde_json::to_value(foo.clone()).unwrap();
    // println!(
    //     "json_expected = {}",
    //     serde_json::to_string_pretty(&json_expected).unwrap()
    // );
    let json_actual = expr_to_json(
        &foo.clone().try_encode(Span::default()).unwrap(),
        &Arc::new(T::to_type()),
    )
    .unwrap();
    // println!(
    //     "json_actual = {}",
    //     serde_json::to_string_pretty(&json_actual).unwrap()
    // );
    assert_eq!(json_expected, json_actual);
    let expr2 = json_to_expr(&json_expected, &Arc::new(T::to_type())).unwrap();
    // println!("expr2 = {}", expr2);
    assert_expr_eq!(expr1, expr2);
}
