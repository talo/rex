use rex_ast::{assert_expr_eq, d, f, n, s, tup, u};
use rex_proc_macro::Rex;
use rex_engine::engine::Builder;
use rex_engine::util::parse_infer_and_eval_with_builder;
use rex_type_system::{
    adt,
    types::{ADTVariant, ADT, ToType, Type},
    tuple,
};

#[allow(dead_code)]
#[test]
pub fn derive_struct() {
    use rex_type_system::{
        adt, adt_variant, bool, float, int, list, string, tuple,
        types::{ToType, Type},
    };

    #[derive(Rex, serde::Deserialize, serde::Serialize)]
    struct MyInnerStruct {
        x: bool,
        y: i64,
        z: f64,
        w: Vec<String>,
        #[serde(rename = "renamed")]
        t: (bool, i64, f64, Vec<String>),
    }

    assert_eq!(
        MyInnerStruct::to_type(),
        Type::ADT(adt!(
            MyInnerStruct = MyInnerStruct {
                x: bool!(),
                y: int!(),
                z: float!(),
                w: list![string!()],
                renamed: tuple!(bool!(), int!(), float!(), list![string!()])
            }
        ))
    );

    #[derive(Rex)]
    struct MyStruct {
        x: bool,
        y: i64,
        z: f64,
        w: Vec<String>,
        t: (bool, i64, f64, Vec<String>),
        u: MyInnerStruct,
        v: Vec<MyInnerStruct>,
    }

    assert_eq!(
        MyStruct::to_type(),
        Type::ADT(adt!(
            MyStruct = MyStruct {
                x: bool!(),
                y: int!(),
                z: float!(),
                w: list![string!()],
                t: tuple!(bool!(), int!(), float!(), list![string!()]),
                u: MyInnerStruct::to_type(),
                v: list![MyInnerStruct::to_type()]
            }
        ))
    );

    #[derive(Rex)]
    struct MyTupleStruct(
        bool,
        i64,
        f64,
        Vec<String>,
        (bool, i64, f64, Vec<String>),
        MyInnerStruct,
        Vec<MyInnerStruct>,
    );

    assert_eq!(
        MyTupleStruct::to_type(),
        Type::ADT(ADT {
            name: String::from("MyTupleStruct"),
            variants: vec![ADTVariant {
                name: String::from("MyTupleStruct"),
                t: Some(Box::new(tuple!(
                    bool!(),
                    int!(),
                    float!(),
                    list![string!()],
                    tuple!(bool!(), int!(), float!(), list![string!()]),
                    MyInnerStruct::to_type(),
                    list![MyInnerStruct::to_type()]
                ))),
                docs: None,
                t_docs: None,
            }],
            docs: None,
        })
    );

    #[derive(Rex)]
    struct MyEmptyStruct {}

    assert_eq!(
        MyEmptyStruct::to_type(),
        Type::ADT(ADT {
            name: String::from("MyEmptyStruct"),
            variants: vec![ADTVariant {
                name: String::from("MyEmptyStruct"),
                t: None,
                docs: None,
                t_docs: None,
            }],
            docs: None,
        })
    );

    #[derive(Rex)]
    struct MyEmptyTupleStruct();

    assert_eq!(
        MyEmptyTupleStruct::to_type(),
        Type::ADT(ADT {
            name: String::from("MyEmptyTupleStruct"),
            variants: vec![ADTVariant {
                name: String::from("MyEmptyTupleStruct"),
                t: None,
                docs: None,
                t_docs: None,
            }],
            docs: None,
        })
    );

    #[derive(Rex)]
    struct MyUnitStruct;

    assert_eq!(
        MyUnitStruct::to_type(),
        Type::ADT(ADT {
            name: String::from("MyUnitStruct"),
            variants: vec![],
            docs: None,
        })
    );
}

#[allow(dead_code)]
#[test]
pub fn derive_enum() {
    use rex_type_system::{
        adt, adt_variant, bool, dict, float, int, list, string, tuple,
        types::{ToType, Type},
    };

    #[derive(Rex, serde::Deserialize, serde::Serialize)]
    struct MyInnerStruct {
        x: bool,
        y: i64,
        z: f64,
        w: Vec<String>,
        t: (bool, i64, f64, Vec<String>),
    }

    assert_eq!(
        MyInnerStruct::to_type(),
        Type::ADT(adt!(
            MyInnerStruct = MyInnerStruct {
                x: bool!(),
                y: int!(),
                z: float!(),
                w: list![string!()],
                t: tuple!(bool!(), int!(), float!(), list![string!()])
            }
        ))
    );

    /// MyEnum has been documented.
    ///
    /// ```rust
    /// MyEnum::X
    /// MyEnum::Y()
    /// MyEnum::Z{}
    /// ```
    ///
    /// These commentsn will appear in the type definition.
    #[derive(Rex, serde::Deserialize, serde::Serialize)]
    enum MyEnum {
        X,
        Y(),

        /// Z has been documented.
        ///
        /// ```rust
        /// MyEnum::Z{}
        /// ```
        Z {},
        W(bool),
        T(i64, f64, Vec<String>),
        U {
            x: bool,
            y: i64,
            z: f64,
            /// This field has been documented.
            w: Vec<String>,
            /// This renamed field has been documented.
            #[serde(rename = "renamed")]
            t: (bool, i64, f64, Vec<String>),
        },
        #[serde(rename = "Renamed")]
        V(MyInnerStruct),
    }

    assert_eq!(
        MyEnum::to_type(),
        Type::ADT(ADT {
            docs: Some(
                r#"MyEnum has been documented.

```rust
MyEnum::X
MyEnum::Y()
MyEnum::Z{}
```

These commentsn will appear in the type definition."#
                    .to_string()
            ),
            name: "MyEnum".to_string(),
            variants: vec![
                ADTVariant {
                    name: "X".to_string(),
                    t: None,
                    docs: None,
                    t_docs: None,
                },
                ADTVariant {
                    name: "Y".to_string(),
                    t: None,
                    docs: None,
                    t_docs: None,
                },
                ADTVariant {
                    docs: Some(
                        r#"Z has been documented.

```rust
MyEnum::Z{}
```"#
                            .to_string()
                    ),
                    name: "Z".to_string(),
                    t: None,
                    t_docs: None,
                },
                ADTVariant {
                    name: "W".to_string(),
                    t: Some(Box::new(bool!())),
                    docs: None,
                    t_docs: None,
                },
                ADTVariant {
                    name: "T".to_string(),
                    t: Some(Box::new(tuple!(int!(), float!(), list![string!()]))),
                    docs: None,
                    t_docs: None,
                },
                ADTVariant {
                    name: "U".to_string(),
                    t: Some(Box::new(dict! {
                        x: bool!(),
                        y: int!(),
                        z: float!(),
                        w: list![string!()],
                        renamed: tuple!(bool!(), int!(), float!(), list![string!()]),
                    })),
                    docs: None,
                    t_docs: Some(
                        [
                            (
                                "w".to_string(),
                                "This field has been documented.".to_string()
                            ),
                            (
                                "renamed".to_string(),
                                "This renamed field has been documented.".to_string()
                            )
                        ]
                        .into_iter()
                        .collect()
                    ),
                },
                ADTVariant {
                    name: "Renamed".to_string(),
                    t: Some(Box::new(MyInnerStruct::to_type())),
                    docs: None,
                    t_docs: None,
                }
            ]
        })
    );
}

#[tokio::test]
async fn adt_enum() {
    #![allow(dead_code)]
    #[derive(Rex)]
    pub enum Color {
        Red,
        Green,
        Blue,
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder.register_adt(&Color::to_type()).unwrap();
    let (res, res_type) = parse_infer_and_eval_with_builder(
        builder,
        r#"(Red, Green, Blue)"#)
        .await
        .unwrap();
    assert_eq!(
        res_type,
        tuple!(Color::to_type(), Color::to_type(), Color::to_type()));
    assert_expr_eq!(
        res,
        tup!(n!("Red", None), n!("Green", None), n!("Blue", None));
        ignore span);
}

#[tokio::test]
async fn adt_enum_int() {
    #![allow(dead_code)]
    #[derive(Rex)]
    pub enum Color {
        Red = 1,
        Green = 2,
        Blue = 3,
    }

    assert_eq!(Color::to_type(), Type::Uint);
}

#[tokio::test]
async fn adt_variant_tuple() {
    #![allow(dead_code)]
    #[derive(Rex)]
    pub enum Shape {
        Rectangle(f64, f64),
        Circle(f64),
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder.register_adt(&Shape::to_type()).unwrap();
    let (res, res_type) = parse_infer_and_eval_with_builder(
        builder,
        r#"Rectangle (2.0 * 3.0) (4.0 * 5.0)"#)
        .await
        .unwrap();
    assert_eq!(res_type, Shape::to_type());
    assert_expr_eq!(
        res,
        n!("Rectangle", Some(tup!(f!(6.0), f!(20.0))));
        ignore span);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder.register_adt(&Shape::to_type()).unwrap();
    let (res, res_type) = parse_infer_and_eval_with_builder(
        builder,
        r#"Circle (3.0 * 4.0)"#)
        .await
        .unwrap();
    assert_eq!(res_type, Shape::to_type());
    assert_expr_eq!(
        res,
        n!("Circle", Some(f!(12.0)));
        ignore span);
}

#[tokio::test]
async fn adt_variant_struct() {
    #![allow(dead_code)]
    #[derive(Rex)]
    pub enum Shape {
        Rectangle { width: f64, height: f64 },
        Circle { radius: f64 },
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder.register_adt(&Shape::to_type()).unwrap();
    let (res, res_type) = parse_infer_and_eval_with_builder(
        builder,
        r#"Rectangle { width = 2.0 * 3.0, height = 4.0 * 5.0 }"#)
        .await
        .unwrap();
    assert_eq!(res_type, Shape::to_type());
    assert_expr_eq!(
        res,
        n!("Rectangle", Some(d!(width = f!(6.0), height = f!(20.0))));
        ignore span);
}

#[tokio::test]
async fn adt_struct() {
    #![allow(dead_code)]
    #[derive(Rex)]
    pub struct Movie {
        pub title: String,
        pub year: u16,
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder.register_adt(&Movie::to_type()).unwrap();
    let (res, res_type) = parse_infer_and_eval_with_builder(
        builder,
        r#"Movie { title = "Godzilla", year = 1954 }"#)
        .await
        .unwrap();
    assert_eq!(res_type, Movie::to_type());
    assert_expr_eq!(
        res,
        n!("Movie", Some(d!(title = s!("Godzilla"), year = u!(1954))));
        ignore span);
}

#[tokio::test]
async fn adt_tuple() {
    #![allow(dead_code)]
    #[derive(Rex)]
    pub struct Movie(pub String, pub u16);

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder.register_adt(&Movie::to_type()).unwrap();
    let (res, res_type) = parse_infer_and_eval_with_builder(
        builder,
        r#"Movie "Godzilla" 1954 }"#)
        .await
        .unwrap();
    assert_eq!(res_type, Movie::to_type());
    assert_expr_eq!(
        res,
        n!("Movie", Some(tup!(s!("Godzilla"), u!(1954))));
        ignore span);
}

#[tokio::test]
async fn adt_curry() {
    #![allow(dead_code)]
    #[derive(Rex)]
    pub enum Shape {
        Rectangle(f64, f64),
        Circle(f64),
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    builder.register_adt(&Shape::to_type()).unwrap();
    let (res, res_type) = parse_infer_and_eval_with_builder(
        builder,
        r#"let partial = Rectangle (2.0 * 3.0) in (partial (3.0 * 4.0), partial (2.0 * 4.0))"#)
        .await
        .unwrap();
    assert_eq!(res_type, tuple!(Shape::to_type(), Shape::to_type()));
    assert_expr_eq!(
        res,
        tup!(
            n!("Rectangle", Some(tup!(f!(6.0), f!(12.0)))),
            n!("Rectangle", Some(tup!(f!(6.0), f!(8.0)))));
        ignore span);
}
