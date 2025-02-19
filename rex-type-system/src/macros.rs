#[macro_export]
macro_rules! bool {
    () => {
        $crate::types::Type::Bool
    };
}

#[macro_export]
macro_rules! uint {
    () => {
        $crate::types::Type::Uint
    };
}

#[macro_export]
macro_rules! int {
    () => {
        $crate::types::Type::Int
    };
}

#[macro_export]
macro_rules! float {
    () => {
        $crate::types::Type::Float
    };
}

#[macro_export]
macro_rules! string {
    () => {
        $crate::types::Type::String
    };
}

#[macro_export]
macro_rules! unresolved {
    ($name:expr) => {
        $crate::types::Type::UnresolvedVar(($name).to_string())
    };
}

#[macro_export]
macro_rules! var {
    ($id:expr) => {
        $crate::types::Type::Var($id)
    };
}

#[macro_export]
macro_rules! result {
    ($t:expr, $e:expr) => {
        $crate::types::Type::Result(Box::new($t), Box::new($e))
    };
}

#[macro_export]
macro_rules! dict {
    ( $($k:expr => $v:expr),* $(,)?) => {
        $crate::types::Type::Dict({
            let mut btree = ::std::collections::BTreeMap::new();
            $(btree.insert(($k).to_string(), $v);)*
            btree
        })
    };
}

#[macro_export]
macro_rules! option {
    ($t:expr) => {
        $crate::types::Type::Option(Box::new($t))
    };
}

#[macro_export]
macro_rules! list {
    ($t:expr) => {
        $crate::types::Type::List(Box::new($t))
    };
}

#[macro_export]
macro_rules! tuple {
    ($($t:expr),* $(,)?) => {
        $crate::types::Type::Tuple(vec![$($t),*])
    };
}

#[macro_export]
macro_rules! adt_variant {
    ($name:ident) => {{
        $crate::types::ADTVariant {
            name: stringify!($name).to_string(),
            t: None,
        }
    }};

    ($name:ident { $($key:ident : $value:expr),* $(,)? }) => {{
        let mut fields = ::std::collections::BTreeMap::new();
        $(fields.insert(stringify!($key).to_string(), $value);)*
        $crate::types::ADTVariant {
            name: stringify!($name).to_string(),
            t: Some(Box::new($crate::types::Type::Dict(fields))),
        }
    }};
}

#[macro_export]
macro_rules! adt {
    ($name:ident = $($v:ident $($vs:tt)?)|*) => {{
        $crate::types::ADT {
            name: stringify!($name).to_string(),
            variants: vec![$(adt_variant!{ $v $($vs)? }),*],
        }
    }};
}

#[macro_export]
macro_rules! arrow {
    ($b:expr) => {
        Type::from($b)
    };

    ($a0:expr => $b:expr) => {
        $crate::types::Type::Arrow(($a0).into(), ($b).into())
    };

    ($a0:expr => $a1:expr => $b:expr) => {
        $crate::types::Type::Arrow(
            ($a0).into(),
            Box::new($crate::types::Type::Arrow(($a1).into(), ($b).into())),
        )
    };

    ($a0:expr => $a1:expr => $a2:expr => $b:expr) => {
        $crate::types::Type::Arrow(
            ($a0).into(),
            Box::new($crate::types::Type::Arrow(
                ($a1).into(),
                Box::new($crate::types::Type::Arrow(($a2).into(), ($b).into())),
            )),
        )
    };

    ($a0:expr => $a1:expr => $a2:expr => $a3:expr => $b:expr) => {
        $crate::types::Type::Arrow(
            ($a0).into(),
            Box::new($crate::types::Type::Arrow(
                ($a1).into(),
                Box::new($crate::types::Type::Arrow(
                    ($a2).into(),
                    Box::new($crate::types::Type::Arrow(($a3).into(), ($b).into())),
                )),
            )),
        )
    };
}

#[cfg(test)]
mod test {
    use rex_ast::id::Id;

    use crate::types::{ADTVariant, Type, ADT};

    #[test]
    fn test_basic_type_macros() {
        assert_eq!(bool!(), Type::Bool);
        assert_eq!(uint!(), Type::Uint);
        assert_eq!(int!(), Type::Int);
        assert_eq!(float!(), Type::Float);
        assert_eq!(string!(), Type::String);
    }

    #[test]
    fn test_var_type_macros() {
        assert_eq!(unresolved!("a"), Type::UnresolvedVar("a".to_string()));
        assert_eq!(unresolved!("b"), Type::UnresolvedVar("b".to_string()));
        assert_eq!(unresolved!("c"), Type::UnresolvedVar("c".to_string()));
        assert_eq!(var!(Id(::std::u64::MIN)), Type::Var(Id(::std::u64::MIN)));
        assert_eq!(var!(Id(::std::u64::MAX)), Type::Var(Id(::std::u64::MAX)));
        assert_eq!(var!(Id(0)), Type::Var(Id(0)));
        assert_eq!(var!(Id(1)), Type::Var(Id(1)));
        assert_eq!(var!(Id(42)), Type::Var(Id(42)));
    }

    #[test]
    fn test_list_type_macro() {
        assert_eq!(list!(float!()), Type::List(Box::new(float!())));
    }

    #[test]
    fn test_tuple_type_macro() {
        // with trailing comma
        assert_eq!(
            tuple!(bool!(), uint!(), int!(), float!(),),
            Type::Tuple(vec![bool!(), uint!(), int!(), float!()])
        );

        // without trailing comma
        assert_eq!(
            tuple!(bool!(), uint!(), int!(), float!()),
            Type::Tuple(vec![bool!(), uint!(), int!(), float!()])
        );
    }
    #[test]
    fn test_dict_type_macro() {
        let c = String::from("c");

        // with trailing comma
        assert_eq!(
            dict!("a" => bool!(), String::from("b") => uint!(), c => int!(),),
            Type::Dict({
                let mut btree = ::std::collections::BTreeMap::new();
                btree.insert("a".to_string(), bool!());
                btree.insert("b".to_string(), uint!());
                btree
            })
        );

        // without trailing comma
        assert_eq!(
            dict!("a" => bool!(), String::from("b") => uint!(), c => int!()),
            Type::Dict({
                let mut btree = ::std::collections::BTreeMap::new();
                btree.insert("a".to_string(), bool!());
                btree.insert("b".to_string(), uint!());
                btree
            })
        );
    }

    #[test]
    fn test_abstract_data_type_macro() {
        let adt = adt! {
            MyADT = MyVariant1 { a: bool!(), b: uint!() }
                  | MyVariant2 { c: string!() }
                  | MyVariant3
        };
        assert_eq!(
            adt,
            ADT {
                name: "MyADT".to_string(),
                variants: vec![
                    ADTVariant {
                        name: "MyVariant1".to_string(),
                        t: Some(Box::new(dict! { "a" => bool!(), "b" => uint!() })),
                    },
                    ADTVariant {
                        name: "MyVariant2".to_string(),
                        t: Some(Box::new(dict! { "c" => string!() })),
                    },
                    ADTVariant {
                        name: "MyVariant3".to_string(),
                        t: None,
                    },
                ],
            }
        );
    }

    #[test]
    fn test_arrow_type_macro() {
        assert_eq!(arrow!(bool!()), Type::Bool);

        assert_eq!(
            arrow!(bool!() => uint!()),
            Type::Arrow(Box::new(bool!()), Box::new(uint!()))
        );

        assert_eq!(
            arrow!(bool!() => uint!() => int!()),
            Type::Arrow(
                Box::new(bool!()),
                Box::new(Type::Arrow(Box::new(uint!()), Box::new(int!())))
            )
        );

        assert_eq!(
            arrow!(bool!() => uint!() => int!() => float!()),
            Type::Arrow(
                Box::new(bool!()),
                Box::new(Type::Arrow(
                    Box::new(uint!()),
                    Box::new(Type::Arrow(Box::new(int!()), Box::new(float!())))
                ))
            )
        );

        assert_eq!(
            arrow!(bool!() => uint!() => int!() => float!() => string!()),
            Type::Arrow(
                Box::new(bool!()),
                Box::new(Type::Arrow(
                    Box::new(uint!()),
                    Box::new(Type::Arrow(
                        Box::new(int!()),
                        Box::new(Type::Arrow(Box::new(float!()), Box::new(string!())))
                    ))
                ))
            )
        );
    }
}
