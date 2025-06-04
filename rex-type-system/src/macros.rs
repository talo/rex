#[macro_export]
macro_rules! bool {
    () => {
        ::std::sync::Arc::new($crate::types::Type::Con($crate::types::TypeCon::Bool))
    };
}

#[macro_export]
macro_rules! uint {
    () => {
        ::std::sync::Arc::new($crate::types::Type::Con($crate::types::TypeCon::Uint))
    };
}

#[macro_export]
macro_rules! int {
    () => {
        ::std::sync::Arc::new($crate::types::Type::Con($crate::types::TypeCon::Int))
    };
}

#[macro_export]
macro_rules! float {
    () => {
        ::std::sync::Arc::new($crate::types::Type::Con($crate::types::TypeCon::Float))
    };
}

#[macro_export]
macro_rules! string {
    () => {
        ::std::sync::Arc::new($crate::types::Type::Con($crate::types::TypeCon::String))
    };
}

#[macro_export]
macro_rules! unresolved {
    ($name:expr) => {
        ::std::sync::Arc::new($crate::types::Type::UnresolvedVar(($name).to_string()))
    };
}

#[macro_export]
macro_rules! var {
    ($id:expr) => {
        ::std::sync::Arc::new($crate::types::Type::Var($id))
    };
}

#[macro_export]
macro_rules! result {
    ($e:expr, $t:expr) => {
        ::std::sync::Arc::new($crate::types::Type::make_result($e, $t))
    };
}

#[macro_export]
macro_rules! dict {
    ( $($k:ident : $v:expr),* $(,)?) => {
        ::std::sync::Arc::new($crate::types::Type::Dict({
            let mut btree = ::std::collections::BTreeMap::new();
            $(btree.insert(stringify!($k).to_string(), $v);)*
            btree
        }))
    };
}

#[macro_export]
macro_rules! option {
    ($t:expr) => {
        ::std::sync::Arc::new($crate::types::Type::App(
            ::std::sync::Arc::new($crate::types::Type::Con($crate::types::TypeCon::Option)),
            $t,
        ))
    };
}

#[macro_export]
macro_rules! list {
    ($t:expr) => {
        ::std::sync::Arc::new($crate::types::Type::App(
            ::std::sync::Arc::new($crate::types::Type::Con($crate::types::TypeCon::List)),
            $t,
        ))
    };
}

#[macro_export]
macro_rules! tuple {
    ($($t:expr),* $(,)?) => {
        ::std::sync::Arc::new($crate::types::Type::Tuple(vec![$($t),*]))
    };
}

#[macro_export]
macro_rules! promise {
    ($t:expr) => {
        ::std::sync::Arc::new($crate::types::Type::App(
            ::std::sync::Arc::new($crate::types::Type::Con($crate::types::TypeCon::Promise)),
            $t,
        ))
    };
}

#[macro_export]
macro_rules! adt_variant {
    ($name:ident) => {{
        $crate::types::ADTVariant {
            name: stringify!($name).to_string(),
            t: None,
            docs: None,
            t_docs: None,
            discriminant: None,
        }
    }};

    ($name:ident { $($key:ident : $value:expr),* $(,)? }) => {{
        let mut fields = ::std::collections::BTreeMap::new();
        $(fields.insert(stringify!($key).to_string(), $value);)*
        $crate::types::ADTVariant {
            name: stringify!($name).to_string(),
            t: Some(::std::sync::Arc::new($crate::types::Type::Dict(fields))),
            docs: None,
            t_docs: None,
            discriminant: None,
        }
    }};

    ($name:ident ( $($value:expr),* $(,)? )) => {{
        let mut fields = Vec::new();
        $(fields.push($value);)*
        $crate::types::ADTVariant {
            name: stringify!($name).to_string(),
            t: Some(::std::sync::Arc::new($crate::types::Type::Tuple(fields))),
            docs: None,
            t_docs: None,
            discriminant: None,
        }
    }};

    ($name:ident .) => {{
        $crate::types::ADTVariant {
            name: stringify!($name).to_string(),
            t: None,
            docs: None,
            t_docs: None,
            discriminant: None,
        }
    }};
}

#[macro_export]
macro_rules! adt {
    ($name:ident = $($v:ident $($vs:tt)?)|*) => {{
        $crate::types::ADT {
            name: stringify!($name).to_string(),
            variants: vec![$(adt_variant!{ $v $($vs)? }),*],
            docs: None,
        }
    }};
}

#[macro_export]
macro_rules! arrow {
    ($b:expr) => {
        $b
    };

    ($a0:expr => $b:expr) => {
        ::std::sync::Arc::new($crate::types::Type::make_arrow(($a0).into(), ($b).into()))
    };

    ($a0:expr => $a1:expr => $b:expr) => {
        ::std::sync::Arc::new($crate::types::Type::make_arrow(
            ($a0).into(),
            ::std::sync::Arc::new($crate::types::Type::make_arrow(($a1).into(), ($b).into())),
        ))
    };

    ($a0:expr => $a1:expr => $a2:expr => $b:expr) => {
        ::std::sync::Arc::new($crate::types::Type::make_arrow(
            ($a0).into(),
            ::std::sync::Arc::new($crate::types::Type::make_arrow(
                ($a1).into(),
                ::std::sync::Arc::new($crate::types::Type::make_arrow(($a2).into(), ($b).into())),
            )),
        ))
    };

    ($a0:expr => $a1:expr => $a2:expr => $a3:expr => $b:expr) => {
        ::std::sync::Arc::new($crate::types::Type::make_arrow(
            ($a0).into(),
            ::std::sync::Arc::new($crate::types::Type::make_arrow(
                ($a1).into(),
                ::std::sync::Arc::new($crate::types::Type::make_arrow(
                    ($a2).into(),
                    ::std::sync::Arc::new($crate::types::Type::make_arrow(
                        ($a3).into(),
                        ($b).into(),
                    )),
                )),
            )),
        ))
    };
}

#[cfg(test)]
mod test {
    use rex_ast::id::Id;
    use uuid::Uuid;

    use crate::types::{ADTVariant, Type, TypeCon, ADT};
    use std::sync::Arc;

    #[test]
    fn test_basic_type_macros() {
        assert_eq!(bool!(), Arc::new(Type::Con(TypeCon::Bool)));
        assert_eq!(uint!(), Arc::new(Type::Con(TypeCon::Uint)));
        assert_eq!(int!(), Arc::new(Type::Con(TypeCon::Int)));
        assert_eq!(float!(), Arc::new(Type::Con(TypeCon::Float)));
        assert_eq!(string!(), Arc::new(Type::Con(TypeCon::String)));
    }

    #[test]
    fn test_var_type_macros() {
        assert_eq!(
            unresolved!("a"),
            Arc::new(Type::UnresolvedVar("a".to_string()))
        );
        assert_eq!(
            unresolved!("b"),
            Arc::new(Type::UnresolvedVar("b".to_string()))
        );
        assert_eq!(
            unresolved!("c"),
            Arc::new(Type::UnresolvedVar("c".to_string()))
        );
        assert_eq!(
            var!(Id(Uuid::default())),
            Arc::new(Type::Var(Id(Uuid::default())))
        );
        assert_eq!(var!(Id(Uuid::max())), Arc::new(Type::Var(Id(Uuid::max()))));
    }

    #[test]
    fn test_list_type_macro() {
        assert_eq!(
            list!(float!()),
            Arc::new(Type::App(Arc::new(Type::Con(TypeCon::List)), float!()))
        );
    }

    #[test]
    fn test_tuple_type_macro() {
        // with trailing comma
        assert_eq!(
            tuple!(bool!(), uint!(), int!(), float!(),),
            Arc::new(Type::Tuple(vec![bool!(), uint!(), int!(), float!()]))
        );

        // without trailing comma
        assert_eq!(
            tuple!(bool!(), uint!(), int!(), float!()),
            Arc::new(Type::Tuple(vec![bool!(), uint!(), int!(), float!()]))
        );
    }
    #[test]
    fn test_dict_type_macro() {
        // with trailing comma
        assert_eq!(
            dict!(a: bool!(), b: uint!(), c: int!(),),
            Arc::new(Type::Dict({
                let mut btree = ::std::collections::BTreeMap::new();
                btree.insert("a".to_string(), bool!());
                btree.insert("b".to_string(), uint!());
                btree.insert("c".to_string(), int!());
                btree
            }))
        );

        // without trailing comma
        assert_eq!(
            dict!(a: bool!(), b: uint!(), c: int!()),
            Arc::new(Type::Dict({
                let mut btree = ::std::collections::BTreeMap::new();
                btree.insert("a".to_string(), bool!());
                btree.insert("b".to_string(), uint!());
                btree.insert("c".to_string(), int!());
                btree
            }))
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
                docs: None,
                name: "MyADT".to_string(),
                variants: vec![
                    ADTVariant {
                        docs: None,
                        name: "MyVariant1".to_string(),
                        t: Some(dict! { a: bool!(), b: uint!() }),
                        t_docs: None,
                        discriminant: None,
                    },
                    ADTVariant {
                        docs: None,
                        name: "MyVariant2".to_string(),
                        t: Some(dict! { c: string!() }),
                        t_docs: None,
                        discriminant: None,
                    },
                    ADTVariant {
                        docs: None,
                        name: "MyVariant3".to_string(),
                        t: None,
                        t_docs: None,
                        discriminant: None,
                    },
                ],
            }
        );
    }

    #[test]
    fn test_arrow_type_macro() {
        assert_eq!(arrow!(bool!()), Arc::new(Type::Con(TypeCon::Bool)));

        assert_eq!(
            arrow!(bool!() => uint!()),
            Arc::new(Type::make_arrow(bool!(), uint!()))
        );

        assert_eq!(
            arrow!(bool!() => uint!() => int!()),
            Arc::new(Type::make_arrow(
                bool!(),
                Arc::new(Type::make_arrow(uint!(), int!()))
            ))
        );

        assert_eq!(
            arrow!(bool!() => uint!() => int!() => float!()),
            Arc::new(Type::make_arrow(
                bool!(),
                Arc::new(Type::make_arrow(
                    uint!(),
                    Arc::new(Type::make_arrow(int!(), float!()))
                ))
            ))
        );

        assert_eq!(
            arrow!(bool!() => uint!() => int!() => float!() => string!()),
            Arc::new(Type::make_arrow(
                bool!(),
                Arc::new(Type::make_arrow(
                    uint!(),
                    Arc::new(Type::make_arrow(
                        int!(),
                        Arc::new(Type::make_arrow(float!(), string!()))
                    ))
                ))
            ))
        );
    }
}
