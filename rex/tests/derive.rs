use rex_proc_macro::Rex;
use rex_type_system::{
    adt,
    types::{ADTVariant, ADT},
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
