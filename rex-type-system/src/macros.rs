#[macro_export]
macro_rules! list {
    ($a:expr) => {
        $crate::types::Type::List(Box::new($a))
    };
}

#[macro_export]
macro_rules! tuple {
    ($($a:expr),*) => {
        $crate::types::Type::Tuple(vec![$($a),*])
    };
}

#[macro_export]
macro_rules! adt_variant_with_named_fields {
    ($name:ident { $($key:ident = $value:expr),* }) => {{
        let mut fields = ::std::collections::HashMap::new();
        $(fields.insert(stringify!($key).to_string(), $value);)*
        $crate::types::ADTVariant {
            name: stringify!($name).to_string(),
            t: Some($crate::types::Dict(fields)),
        }
    }};
}

#[macro_export]
macro_rules! adt {
    ($name:ident = | $($v:ident $vs:tt)|*) => {{
        adt!{ $name = $($v $vs)|* }
    }};
    ($name:ident = $($v:ident $vs:tt)|*) => {{
        $crate::types::ADT {
            name: stringify!($name).to_string(),
            variants: vec![$(adt_variant_with_named_fields!{ $v $vs }),*],
        }
    }};
}

#[macro_export]
macro_rules! arrow {
    ($a:expr => $b:expr) => {
        $crate::types::Type::Arrow(($a).into(), ($b).into())
    };
}
