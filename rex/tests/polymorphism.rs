use rex_ast::expr::Expr;
use rex_ast::id::Id;
use rex_engine::{engine::Builder, error::Error};
use rex_lexer::span::Span;
use rex_proc_macro::Rex;
use rex_type_system::{
    types::{ToType, Type},
    uint,
};
use std::sync::Arc;

#[tokio::test]
async fn test_function_overload_param_count_mismatch() {
    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    let res = builder.register_fn_core_with_name(
        "map",
        Type::build_arrow(vec![uint!(), uint!(), uint!()], uint!()),
        Box::new(move |_ctx, _args| {
            Box::pin(async move { Ok(Expr::Uint(Id::new(), Span::default(), 0)) })
        }),
    );

    assert_eq!(
        res,
        Err(Error::OverloadParamCountMismatch {
            name: "map".to_string(),
            new: 3,
            existing: 2,
        })
    );
}

#[tokio::test]
async fn test_accessor_overload_param_count_mismatch() {
    #[derive(Rex)]
    struct Foo {
        a: u64,
        map: String,
    }

    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    let r = builder.register_adt(&Arc::new(Foo::to_type()), None, None);
    assert_eq!(
        r,
        Err(Error::OverloadParamCountMismatch {
            name: "map".to_string(),
            new: 1,
            existing: 2,
        })
    );
}
