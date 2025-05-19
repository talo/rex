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
    constraint::{generate_constraints, ConstraintSystem},
    types::Type,
    unify,
    // error::TypeError,
};
use std::{collections::BTreeSet, sync::Arc};

pub struct Program<State>
where
    State: Clone + Send + Sync + 'static,
{
    pub ftable: Ftable<State>,
    pub res_type: Arc<Type>,
    pub expr: Expr,
    pub subst: Subst,
}

impl<State> Program<State>
where
    State: Clone + Send + Sync + 'static,
{
    pub fn compile(builder: Builder<State>, code: &str) -> Result<Self, Error> {
        let tokens = Token::tokenize(code).map_err(|e| match e {
            LexicalError::UnexpectedToken(s) => Error::UnexpectedToken {
                span: s,
                trace: Default::default(),
            },
        })?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_program().map_err(|es| Error::Parser {
            errors: es,
            trace: Default::default(),
        })?;

        let (ftable, type_env) = builder.build();
        let mut constraint_system = ConstraintSystem::default();

        let mut errors = BTreeSet::new();
        let ty = generate_constraints(&expr, &type_env, &mut constraint_system, &mut errors);
        let subst = unify::unify_constraints(&constraint_system, &mut errors);
        if !errors.is_empty() {
            // Sort by span and path first, so the errors appear in the same order the parts
            // of the input file they correspond do. The default order for an enum produced by
            // #[derive(Ord)] sorts by type first, which is not the best choice for this case.
            let mut errors = errors.into_iter().collect::<Vec<_>>();
            errors.sort_by(|a, b| (a.span(), a.path(), a).cmp(&(b.span(), b.path(), b)));

            return Err(Error::TypeInference {
                errors,
                trace: Default::default(),
            });
        }

        let res_type = unify::apply_subst(&ty, &subst);
        Ok(Program {
            ftable,
            res_type,
            expr,
            subst,
        })
    }

    pub async fn run(self, state: State) -> Result<Expr, Error> {
        eval(
            &Context {
                scope: Scope::new_sync(),
                ftable: Arc::new(self.ftable),
                state: Arc::new(state),
            },
            &self.expr,
            None,
        )
        .await
    }
}
