use crate::engine::Builder;
use crate::error::Error;
use crate::eval::eval;
use crate::eval::Context;
use crate::ftable::Ftable;
use rex_ast::expr::{Expr, Scope};
use rex_lexer::LexicalError;
use rex_lexer::Token;
use rex_parser::Parser;
use rex_type_system::unify::Subst;
use rex_type_system::{
    constraint::generate_constraints,
    types::{ExprTypeEnv, Type},
    unify,
};
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct Program<State>
where
    State: Clone + Send + Sync + 'static,
{
    pub ftable: Ftable<State>,
    pub res_type: Type,
    pub expr: Expr,
    pub expr_type_env: ExprTypeEnv,
    pub subst: Subst,
    pub trace_eval: bool,
}

impl<State> Program<State>
where
    State: Clone + Send + Sync + 'static,
{
    pub fn compile(builder: Builder<State>, code: &str) -> Result<Self, Error> {
        let tokens = Token::tokenize(code).map_err(|e| match e {
            LexicalError::UnexpectedToken(s) => Error::UnexpectedToken(s),
        })?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().map_err(|e| match e {
            rex_parser::error::Error::Parser(e) => Error::Parser(e),
        })?;

        let (mut constraint_system, ftable, type_env) = builder.build();

        let mut expr_type_env = ExprTypeEnv::new();
        let ty = generate_constraints(&expr, &type_env, &mut expr_type_env, &mut constraint_system)
            .map_err(|e| Error::TypeInference(e))?;
        let subst =
            unify::unify_constraints(&constraint_system).map_err(|e| Error::TypeInference(e))?;

        let res_type = unify::apply_subst(&ty, &subst);
        Ok(Program {
            ftable,
            res_type,
            expr,
            expr_type_env,
            subst,
            trace_eval: false,
        })
    }

    pub async fn run(self, state: State) -> Result<Expr, Error> {
        eval(
            &Context {
                scope: Scope::new_sync(),
                ftable: self.ftable,
                subst: self.subst,
                env: Arc::new(RwLock::new(self.expr_type_env)),
                state: state,
                trace_eval: self.trace_eval,
            },
            &self.expr,
        )
        .await
    }
}
