use rex_ast::{assert_expr_eq, l, u};
use rex_engine::{engine::Builder, error::Error, program::Program};
use rex_type_system::{list, uint};

#[tokio::test]
async fn chunk_even_partitions() {
    let builder: Builder<()> = Builder::with_prelude().unwrap();
    let program = Program::compile(
        builder,
        r#"
        chunk 2 [1, 2, 3, 4]
        "#,
    )
    .unwrap();

    assert_eq!(program.res_type, list!(list!(uint!())));

    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, l!(l!(u!(1), u!(2)), l!(u!(3), u!(4))); ignore span);
}

#[tokio::test]
async fn chunk_handles_remainder() {
    let builder: Builder<()> = Builder::with_prelude().unwrap();
    let program = Program::compile(
        builder,
        r#"
        chunk 3 [1, 2, 3, 4, 5]
        "#,
    )
    .unwrap();

    let res = program.run(()).await.unwrap();
    assert_expr_eq!(res, l!(l!(u!(1), u!(2), u!(3)), l!(u!(4), u!(5))); ignore span);
}

#[tokio::test]
async fn chunk_rejects_zero_size() {
    let builder: Builder<()> = Builder::with_prelude().unwrap();
    let program = Program::compile(
        builder,
        r#"
        chunk 0 [1]
        "#,
    )
    .unwrap();

    let res = program.run(()).await;
    match res {
        Err(Error::Custom { error, .. }) => {
            assert_eq!(error, "Chunk size must be greater than zero");
        }
        other => panic!("unexpected result: {:?}", other),
    }
}
