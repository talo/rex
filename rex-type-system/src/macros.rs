#[macro_export]
macro_rules! bool {
    () => {
        ::std::sync::Arc::new($crate::types::Type::Bool)
    };
}

#[macro_export]
macro_rules! uint {
    () => {
        ::std::sync::Arc::new($crate::types::Type::Uint)
    };
}

#[macro_export]
macro_rules! int {
    () => {
        ::std::sync::Arc::new($crate::types::Type::Int)
    };
}

#[macro_export]
macro_rules! float {
    () => {
        ::std::sync::Arc::new($crate::types::Type::Float)
    };
}

#[macro_export]
macro_rules! string {
    () => {
        ::std::sync::Arc::new($crate::types::Type::String)
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
    ($t:expr, $e:expr) => {
        ::std::sync::Arc::new($crate::types::Type::Result($t, $e))
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
        ::std::sync::Arc::new($crate::types::Type::Option($t))
    };
}

#[macro_export]
macro_rules! list {
    ($t:expr) => {
        ::std::sync::Arc::new($crate::types::Type::List($t))
    };
}

#[macro_export]
macro_rules! tuple {
    ($($t:expr),* $(,)?) => {
        ::std::sync::Arc::new($crate::types::Type::Tuple(vec![$($t),*]))
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
        }
    }};

    ($name:ident .) => {{
        $crate::types::ADTVariant {
            name: stringify!($name).to_string(),
            t: None,
            docs: None,
            t_docs: None,
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
        ::std::sync::Arc::new($crate::types::Type::Arrow(($a0).into(), ($b).into()))
    };

    ($a0:expr => $a1:expr => $b:expr) => {
        ::std::sync::Arc::new($crate::types::Type::Arrow(
            ($a0).into(),
            ::std::sync::Arc::new($crate::types::Type::Arrow(($a1).into(), ($b).into())),
        ))
    };

    ($a0:expr => $a1:expr => $a2:expr => $b:expr) => {
        ::std::sync::Arc::new($crate::types::Type::Arrow(
            ($a0).into(),
            ::std::sync::Arc::new($crate::types::Type::Arrow(
                ($a1).into(),
                ::std::sync::Arc::new($crate::types::Type::Arrow(($a2).into(), ($b).into())),
            )),
        ))
    };

    ($a0:expr => $a1:expr => $a2:expr => $a3:expr => $b:expr) => {
        ::std::sync::Arc::new($crate::types::Type::Arrow(
            ($a0).into(),
            ::std::sync::Arc::new($crate::types::Type::Arrow(
                ($a1).into(),
                ::std::sync::Arc::new($crate::types::Type::Arrow(
                    ($a2).into(),
                    ::std::sync::Arc::new($crate::types::Type::Arrow(($a3).into(), ($b).into())),
                )),
            )),
        ))
    };
}

#[cfg(test)]
mod test {
    use rex_ast::id::Id;
    use uuid::Uuid;

    use crate::types::{ADTVariant, Type, ADT};
    use std::sync::Arc;

    #[test]
    fn test_basic_type_macros() {
        assert_eq!(bool!(), Arc::new(Type::Bool));
        assert_eq!(uint!(), Arc::new(Type::Uint));
        assert_eq!(int!(), Arc::new(Type::Int));
        assert_eq!(float!(), Arc::new(Type::Float));
        assert_eq!(string!(), Arc::new(Type::String));
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
        assert_eq!(list!(float!()), Arc::new(Type::List(float!())));
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
                    },
                    ADTVariant {
                        docs: None,
                        name: "MyVariant2".to_string(),
                        t: Some(dict! { c: string!() }),
                        t_docs: None,
                    },
                    ADTVariant {
                        docs: None,
                        name: "MyVariant3".to_string(),
                        t: None,
                        t_docs: None,
                    },
                ],
            }
        );
    }

    #[test]
    fn test_arrow_type_macro() {
        assert_eq!(arrow!(bool!()), Arc::new(Type::Bool));

        assert_eq!(
            arrow!(bool!() => uint!()),
            Arc::new(Type::Arrow(bool!(), uint!()))
        );

        assert_eq!(
            arrow!(bool!() => uint!() => int!()),
            Arc::new(Type::Arrow(bool!(), Arc::new(Type::Arrow(uint!(), int!()))))
        );

        assert_eq!(
            arrow!(bool!() => uint!() => int!() => float!()),
            Arc::new(Type::Arrow(
                bool!(),
                Arc::new(Type::Arrow(
                    uint!(),
                    Arc::new(Type::Arrow(int!(), float!()))
                ))
            ))
        );

        assert_eq!(
            arrow!(bool!() => uint!() => int!() => float!() => string!()),
            Arc::new(Type::Arrow(
                bool!(),
                Arc::new(Type::Arrow(
                    uint!(),
                    Arc::new(Type::Arrow(
                        int!(),
                        Arc::new(Type::Arrow(float!(), string!()))
                    ))
                ))
            ))
        );
    }
}
