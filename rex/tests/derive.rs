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
                )))
            }]
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
                t: None
            }]
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
                t: None
            }]
        })
    );

    #[derive(Rex)]
    struct MyUnitStruct;

    assert_eq!(
        MyUnitStruct::to_type(),
        Type::ADT(ADT {
            name: String::from("MyUnitStruct"),
            variants: vec![ADTVariant {
                name: String::from("MyUnitStruct"),
                t: None
            }]
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

    #[derive(Rex, serde::Deserialize, serde::Serialize)]
    enum MyEnum {
        X,
        Y(),
        Z {},
        W(bool),
        T(i64, f64, Vec<String>),
        U {
            x: bool,
            y: i64,
            z: f64,
            w: Vec<String>,
            #[serde(rename = "renamed")]
            t: (bool, i64, f64, Vec<String>),
        },
        #[serde(rename = "Renamed")]
        V(MyInnerStruct),
    }

    assert_eq!(
        MyEnum::to_type(),
        Type::ADT(ADT {
            name: "MyEnum".to_string(),
            variants: vec![
                ADTVariant {
                    name: "X".to_string(),
                    t: None,
                },
                ADTVariant {
                    name: "Y".to_string(),
                    t: None,
                },
                ADTVariant {
                    name: "Z".to_string(),
                    t: None,
                },
                ADTVariant {
                    name: "W".to_string(),
                    t: Some(Box::new(bool!())),
                },
                ADTVariant {
                    name: "T".to_string(),
                    t: Some(Box::new(tuple!(int!(), float!(), list![string!()]))),
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
                },
                ADTVariant {
                    name: "Renamed".to_string(),
                    t: Some(Box::new(MyInnerStruct::to_type())),
                }
            ]
        })
    );
}
