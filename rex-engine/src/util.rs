use std::sync::Arc;
use tokio::sync::RwLock;
use crate::engine::Builder;
use crate::error::Error;
use crate::eval::Context;
use crate::eval::eval;
use rex_ast::expr::{Expr, Scope};
use rex_lexer::Token;
use rex_parser::Parser;
use rex_type_system::{
    constraint::generate_constraints,
    trace::sprint_expr_with_type,
    types::{ExprTypeEnv, Type},
    unify,
};

/// Helper function for parsing, inferring, and evaluating a given code
/// snippet. Pretty much all of the test suites can use this flow for
/// testing that the engine is correctly evaluating types and expressions.
/// In the future, we should probably make this (or some version of this) an
/// actual function for library users too.
pub async fn parse_infer_and_eval(code: &str) -> Result<(Expr, Type), Error> {
    let builder: Builder<()> = Builder::with_prelude().unwrap();
    parse_infer_and_eval_b(builder, code).await
}

/// This is similar to parse_infer_and_eval but lets you supply your own Builder,
/// in case you want to register any extra functions or ADTs.
pub async fn parse_infer_and_eval_b(
    builder: Builder<()>,
    code: &str,
) -> Result<(Expr, Type), Error> {
    let mut parser = Parser::new(Token::tokenize(code).unwrap());
    let expr = parser.parse_expr().unwrap();

    let (mut constraint_system, ftable, type_env) = builder.build();

    let mut expr_type_env = ExprTypeEnv::new();
    let ty = generate_constraints(&expr, &type_env, &mut expr_type_env, &mut constraint_system)
        .unwrap();

    let subst = unify::unify_constraints(&constraint_system).unwrap();
    let res_type = unify::apply_subst(&ty, &subst);

    println!(
        "{}\n",
        sprint_expr_with_type(&expr, &expr_type_env, Some(&subst))
    );

    let res = eval(
        &Context {
            scope: Scope::new_sync(),
            ftable,
            subst,
            env: Arc::new(RwLock::new(expr_type_env)),
            state: (),
        },
        &expr,
    )
    .await;

    res.map(|res| (res, res_type))
}
