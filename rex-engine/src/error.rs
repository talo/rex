use rex_ast::{
    expr::{Expr, Var},
    id::Id,
};
use rex_lexer::span::Span;
use rex_parser::error::ParserErr;
use rex_type_system::types::{Type, TypeScheme, ADT};
use serde_json::Value;
use std::{collections::VecDeque, sync::Arc};

// TODO(loong): re-implement traces so that developers can get meaningful
// errors when something goes wrong.
#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum Error {
    #[error("ambiguous overload of function {0}: types {1} and {2} may overlap")]
    OverlappingFunctions(String, TypeScheme, TypeScheme),
    #[error("unexpected token {0}")]
    UnexpectedToken(Span),
    #[error("parsing failed: {0:?}")]
    Parser(Vec<ParserErr>),
    #[error("type inference failed: {0:?}")]
    TypeInference(String),
    #[error("variable not found {var}")]
    VarNotFound { var: Var },
    #[error("expected {expected}, got {got}")]
    ExpectedTypeGotValue { expected: Arc<Type>, got: Expr },
    #[error("expected {expected}, got {got}")]
    ExpectedTypeGotJSON { expected: Arc<Type>, got: Value },
    #[error("missing argument {argument}")]
    MissingArgument { argument: usize },
    #[error("{0}")]
    ParseIntError(#[from] std::num::ParseIntError),
    #[error("{0}")]
    ParseFloatError(#[from] std::num::ParseFloatError),
    #[error("cannot overload a parametrically polymorphic function")]
    ParametricOverload {
        name: String,
        prev_insts: Vec<Arc<Type>>,
        curr_inst: Arc<Type>,
    },
    #[error("{0}")]
    RegexCompilationError(#[from] regex::Error),
    #[error("Type of Expr {0} is unknown")]
    ExprTypeUnknown(Id),
    #[error("Different ADTs found with same name {name:?}: new {new}, existing {existing}")]
    ADTNameConflict {
        name: String,
        new: ADT,
        existing: ADT,
    },
    #[error("Overloaded function {name:?} has {new} params; existing has {existing}")]
    OverloadParamCountMismatch {
        name: String,
        new: usize,
        existing: usize,
    },
    #[error("{error}")]
    Custom {
        error: String,
        trace: VecDeque<Expr>,
    },
}
