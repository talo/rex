use rex::ast::{assert_expr_eq, b, d, f, n, s, tup, u};
use rex::engine::codec::{Decode, Encode};
use rex::lexer::span::Span;
use rex::type_system::types::{ToType, Type, ADT};
use rex::type_system::{adt, adt_variant, bool, float, string, uint};
use rex::Rex;

#[test]
fn derive_codec_enum_unit() {
    #[derive(Rex, Debug, PartialEq)]
    enum Foo {
        One,
        Two,
    }

    assert_eq!(Foo::to_type(), Type::ADT(adt! { Foo = One . | Two . }));

    let expr = Foo::One.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, n!("One", None));
    let expr = Foo::Two.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, n!("Two", None));

    let decoded = Foo::try_decode(&n!("One", None)).unwrap();
    assert_eq!(decoded, Foo::One);
    let decoded = Foo::try_decode(&n!("Two", None)).unwrap();
    assert_eq!(decoded, Foo::Two);
}

#[test]
fn derive_codec_enum_named_fields() {
    #[derive(Rex, Debug, PartialEq)]
    enum Foo {
        One { a: u64, b: String },
        Two { c: bool, d: f64 },
    }

    assert_eq!(
        Foo::to_type(),
        Type::ADT(adt! {
            Foo = One { a: uint!(), b: string!() }
                | Two { c: bool!(), d: float!() }
        })
    );

    let foo = Foo::One {
        a: 42,
        b: "Hello".to_string(),
    };
    let expr = foo.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, n!("One", Some(d!(a = u!(42), b = s!("Hello")))));
    let decoded = Foo::try_decode(&n!("One", Some(d!(a = u!(42), b = s!("Hello"))))).unwrap();
    assert_eq!(
        decoded,
        Foo::One {
            a: 42,
            b: "Hello".to_string()
        }
    );

    let foo = Foo::Two { c: true, d: 2.5 };
    let expr = foo.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, n!("Two", Some(d!(c = b!(true), d = f!(2.5)))));
    let decoded = Foo::try_decode(&n!("Two", Some(d!(c = b!(true), d = f!(2.5))))).unwrap();
    assert_eq!(decoded, Foo::Two { c: true, d: 2.5 });
}

#[test]
fn derive_codec_enum_unnamed_fields() {
    #[derive(Rex, Debug, PartialEq)]
    enum Foo {
        One(u64, String),
        Two(bool, f64, u64),
    }

    assert_eq!(
        Foo::to_type(),
        Type::ADT(adt! {
            Foo = One ( uint!(), string!() )
                | Two ( bool!(), float!(), uint!() )
        })
    );

    let foo = Foo::One(42, "Hello".to_string());
    let expr = foo.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, n!("One", Some(tup!(u!(42), s!("Hello")))));
    let decoded = Foo::try_decode(&n!("One", Some(tup!(u!(42), s!("Hello"))))).unwrap();
    assert_eq!(decoded, Foo::One(42, "Hello".to_string()));

    let foo = Foo::Two(true, 2.5, 99);
    let expr = foo.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, n!("Two", Some(tup!(b!(true), f!(2.5), u!(99)))));
    let decoded = Foo::try_decode(&n!("Two", Some(tup!(b!(true), f!(2.5), u!(99))))).unwrap();
    assert_eq!(decoded, Foo::Two(true, 2.5, 99));
}

#[test]
fn derive_codec_enum_mixed() {
    #[derive(Rex, Debug, PartialEq)]
    enum Foo {
        One,
        Two { a: u64, b: String },
        Three(bool, f64, u64),
    }

    assert_eq!(
        Foo::to_type(),
        Type::ADT(adt!(
            Foo = One .
                | Two { a: uint!(), b: string!() }
                | Three ( bool!(), float!(), uint!() )
        ))
    );

    let foo = Foo::One;
    let expr = foo.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, n!("One", None));
    let decoded = Foo::try_decode(&n!("One", None)).unwrap();
    assert_eq!(decoded, Foo::One);

    let foo = Foo::Two {
        a: 42,
        b: "Hello".to_string(),
    };
    let expr = foo.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, n!("Two", Some(d!(a = u!(42), b = s!("Hello")))));
    let decoded = Foo::try_decode(&n!("Two", Some(d!(a = u!(42), b = s!("Hello"))))).unwrap();
    assert_eq!(
        decoded,
        Foo::Two {
            a: 42,
            b: "Hello".to_string()
        }
    );

    let foo = Foo::Three(true, 2.5, 99);
    let expr = foo.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, n!("Three", Some(tup!(b!(true), f!(2.5), u!(99)))));
    let decoded = Foo::try_decode(&n!("Three", Some(tup!(b!(true), f!(2.5), u!(99))))).unwrap();
    assert_eq!(decoded, Foo::Three(true, 2.5, 99));
}

#[test]
fn derive_codec_struct_named_fields() {
    #[derive(Rex, Debug, PartialEq, Eq)]
    pub struct Foo {
        pub a: u64,
        pub b: String,
    }

    assert_eq!(
        Foo::to_type(),
        Type::ADT(adt! { Foo = Foo { a: uint!(), b: string!() }})
    );

    let foo = Foo {
        a: 42,
        b: "Hello".to_string(),
    };
    let expr = foo.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, d!(a = u!(42), b = s!("Hello")));

    let decoded = Foo::try_decode(&d!(a = u!(42), b = s!("Hello"))).unwrap();
    assert_eq!(
        decoded,
        Foo {
            a: 42,
            b: "Hello".to_string()
        }
    );
}

#[test]
fn derive_codec_struct_unnamed_fields() {
    #[derive(Rex, Debug, PartialEq)]
    pub struct Foo(u64, String);

    assert_eq!(
        Foo::to_type(),
        Type::ADT(adt! { Foo = Foo ( uint!(), string!() )})
    );

    let foo = Foo(42, "Hello".to_string());
    let expr = foo.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, tup!(u!(42), s!("Hello")));
    let decoded = Foo::try_decode(&tup!(u!(42), s!("Hello"))).unwrap();
    assert_eq!(decoded, Foo(42, "Hello".to_string()));
}

#[test]
fn derive_codec_struct_unit() {
    #[derive(Rex, Debug, PartialEq)]
    pub struct Foo;

    assert_eq!(
        Foo::to_type(),
        Type::ADT(ADT {
            name: "Foo".to_string(),
            docs: None,
            variants: vec![]
        })
    );

    let foo = Foo;
    let expr = foo.try_encode(Span::default()).unwrap();
    assert_expr_eq!(expr, tup!());

    let decoded = Foo::try_decode(&tup!()).unwrap();
    assert_eq!(decoded, Foo);
}
