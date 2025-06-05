use crate::eval::Stack;
use rex_ast::expr::{Expr, Var};
use rex_lexer::span::Span;
use rex_parser::error::ParserErr;
use rex_type_system::{
    error::TypeError,
    types::{Type, TypeScheme, ADT},
};
use serde_json::Value;
use std::{fmt, sync::Arc};

// TODO(loong): re-implement traces so that developers can get meaningful
// errors when something goes wrong.
#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum Error {
    #[error("ambiguous overload of function {name}: types {t1} and {t2} may overlap{trace}")]
    OverlappingFunctions {
        name: String,
        t1: TypeScheme,
        t2: TypeScheme,
        trace: Trace,
    },
    #[error("unexpected token {span}{trace}")]
    UnexpectedToken { span: Span, trace: Trace },
    #[error("{msg}", msg = parse_error_msg(.errors))]
    Parser {
        errors: Vec<ParserErr>,
        trace: Trace,
    },
    #[error("{msg}{trace}", msg=type_error_msg(.errors))]
    TypeInference {
        errors: Vec<TypeError>,
        trace: Trace,
    },
    #[error("variable not found {var}{trace}")]
    VarNotFound { var: Var, trace: Trace },
    #[error("expected {expected}, got {got}{trace}")]
    ExpectedTypeGotValue {
        expected: Arc<Type>,
        got: Arc<Expr>,
        trace: Trace,
    },
    #[error("expected {expected}, got {got}{trace}")]
    ExpectedTypeGotJSON {
        expected: Arc<Type>,
        got: Value,
        trace: Trace,
    },
    #[error("missing argument {argument}{trace}")]
    MissingArgument { argument: usize, trace: Trace },
    #[error("{error}{trace}")]
    ParseIntError {
        error: std::num::ParseIntError,
        trace: Trace,
    },
    #[error("{error}{trace}")]
    ParseFloatError {
        error: std::num::ParseFloatError,
        trace: Trace,
    },
    #[error("{error}{trace}")]
    RegexCompilationError { error: regex::Error, trace: Trace },
    #[error("Different ADTs found with same name {name:?}: new {new}, existing {existing}{trace}")]
    ADTNameConflict {
        name: String,
        new: Arc<ADT>,
        existing: Arc<ADT>,
        trace: Trace,
    },
    #[error("Overloaded function {name:?} has {new} params; existing has {existing}{trace}")]
    OverloadParamCountMismatch {
        name: String,
        new: usize,
        existing: usize,
        trace: Trace,
    },
    #[error("{error}{trace}")]
    Custom { error: String, trace: Trace },
}

impl Error {
    pub fn with_extra_trace(self, stack: Option<&Stack<'_>>) -> Error {
        match self {
            Self::OverlappingFunctions {
                name,
                t1,
                t2,
                trace,
            } => Self::OverlappingFunctions {
                name,
                t1,
                t2,
                trace: trace.extend(stack),
            },
            Self::UnexpectedToken { span, trace } => Self::UnexpectedToken {
                span,
                trace: trace.extend(stack),
            },
            Self::Parser { errors, trace } => Self::Parser {
                errors,
                trace: trace.extend(stack),
            },
            Self::TypeInference { errors, trace } => Self::TypeInference {
                errors,
                trace: trace.extend(stack),
            },
            Self::VarNotFound { var, trace } => Self::VarNotFound {
                var,
                trace: trace.extend(stack),
            },
            Self::ExpectedTypeGotValue {
                expected,
                got,
                trace,
            } => Self::ExpectedTypeGotValue {
                expected,
                got,
                trace: trace.extend(stack),
            },
            Self::ExpectedTypeGotJSON {
                expected,
                got,
                trace,
            } => Self::ExpectedTypeGotJSON {
                expected,
                got,
                trace: trace.extend(stack),
            },
            Self::MissingArgument { argument, trace } => Self::MissingArgument {
                argument,
                trace: trace.extend(stack),
            },
            Self::ParseIntError { error, trace } => Self::ParseIntError {
                error,
                trace: trace.extend(stack),
            },
            Self::ParseFloatError { error, trace } => Self::ParseFloatError {
                error,
                trace: trace.extend(stack),
            },
            Self::RegexCompilationError { error, trace } => Self::RegexCompilationError {
                error,
                trace: trace.extend(stack),
            },
            Self::ADTNameConflict {
                name,
                new,
                existing,
                trace,
            } => Self::ADTNameConflict {
                name,
                new,
                existing,
                trace: trace.extend(stack),
            },
            Self::OverloadParamCountMismatch {
                name,
                new,
                existing,
                trace,
            } => Self::OverloadParamCountMismatch {
                name,
                new,
                existing,
                trace: trace.extend(stack),
            },
            Self::Custom { error, trace } => Self::Custom {
                error,
                trace: trace.extend(stack),
            },
        }
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(e: std::num::ParseIntError) -> Self {
        Error::ParseIntError {
            error: e,
            trace: Trace::default(),
        }
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(e: std::num::ParseFloatError) -> Self {
        Error::ParseFloatError {
            error: e,
            trace: Default::default(),
        }
    }
}

impl From<regex::Error> for Error {
    fn from(e: regex::Error) -> Self {
        Error::RegexCompilationError {
            error: e,
            trace: Default::default(),
        }
    }
}

impl From<&str> for Error {
    fn from(msg: &str) -> Self {
        Error::Custom {
            error: msg.to_string(),
            trace: Default::default(),
        }
    }
}

impl From<String> for Error {
    fn from(msg: String) -> Self {
        Error::Custom {
            error: msg,
            trace: Default::default(),
        }
    }
}

fn parse_error_msg(errors: &[ParserErr]) -> String {
    let mut res = String::new();
    for (i, e) in errors.iter().enumerate() {
        if i > 0 {
            res.push('\n');
        }
        res.push_str(&e.to_string());
    }
    res
}

fn type_error_msg(errors: &[TypeError]) -> String {
    let mut res = String::new();
    for (i, e) in errors.iter().enumerate() {
        if i > 0 {
            res.push('\n');
        }
        res.push_str(&e.to_string());
    }
    res
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Trace(pub Vec<Span>);

impl From<Option<&Stack<'_>>> for Trace {
    fn from(stack: Option<&Stack<'_>>) -> Self {
        Trace(Stack::to_vec(stack))
    }
}

impl Trace {
    pub fn extend(self, stack: Option<&Stack<'_>>) -> Trace {
        let mut spans = self.0;
        spans.extend(Stack::to_vec(stack));
        Trace(spans)
    }
}

impl fmt::Display for Trace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        for span in &self.0 {
            write!(f, "\n{}", span)?;
        }
        Ok(())
    }
}
