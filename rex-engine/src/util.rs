use crate::engine::Builder;
use crate::error::Error;
use crate::program::Program;
use rex_ast::expr::Expr;
use rex_type_system::types::Type;

/// Helper function for parsing, inferring, and evaluating a given code
/// snippet. Pretty much all of the test suites can use this flow for
/// testing that the engine is correctly evaluating types and expressions.
/// In the future, we should probably make this (or some version of this) an
/// actual function for library users too.
pub async fn parse_infer_and_eval(code: &str) -> Result<(Expr, Type), Error> {
    let builder: Builder<()> = Builder::with_prelude().unwrap();
    parse_infer_and_eval_with_builder(builder, code).await
}

/// This is similar to parse_infer_and_eval but lets you supply your own Builder,
/// in case you want to register any extra functions or ADTs.
pub async fn parse_infer_and_eval_with_builder(
    builder: Builder<()>,
    code: &str,
) -> Result<(Expr, Type), Error> {
    let program = Program::compile(builder, code)?;
    let res_type = program.res_type.clone();
    let res = program.run(()).await;

    res.map(|res| (res, res_type))
}
