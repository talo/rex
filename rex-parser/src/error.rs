use std::fmt::{self, Display, Formatter};

use rex_ast::span::Span;

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("parse errors {0:?}")]
    Parser(Vec<ParserErr>),
}

impl From<Vec<ParserErr>> for Error {
    fn from(errors: Vec<ParserErr>) -> Error {
        Error::Parser(errors)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParserErr {
    span: Span,
    message: String,
}

impl ParserErr {
    pub fn new(span: Span, message: String) -> ParserErr {
        ParserErr { span, message }
    }
}

impl Display for ParserErr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.span.begin, self.message)
    }
}

impl From<String> for ParserErr {
    fn from(message: String) -> ParserErr {
        ParserErr {
            span: Span::new(0, 0, 0, 0),
            message,
        }
    }
}

impl From<&str> for ParserErr {
    fn from(message: &str) -> ParserErr {
        ParserErr {
            span: Span::new(0, 0, 0, 0),
            message: message.to_string(),
        }
    }
}
