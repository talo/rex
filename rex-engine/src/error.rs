use std::collections::VecDeque;

use rex_ast::{ast::AST, id::Id, types::Type};

use crate::value::{Function, Value};

pub trait Trace {
    fn trace(self, node: AST) -> Self;
}

impl<T> Trace for Result<T, Error> {
    fn trace(self, node: AST) -> Self {
        match self {
            Ok(ok) => Ok(ok),
            Err(Error::ExpectedCallable { got, mut trace }) => Err(Error::ExpectedCallable {
                got,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::ExpectedNum { got, mut trace }) => Err(Error::ExpectedNum {
                got,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::ExpectedCmp { got, mut trace }) => Err(Error::ExpectedCmp {
                got,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::ExpectedConcat { got, mut trace }) => Err(Error::ExpectedConcat {
                got,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::ExpectedIndexable { got, mut trace }) => Err(Error::ExpectedIndexable {
                got,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::ExpectedKeyable { got, mut trace }) => Err(Error::ExpectedKeyable {
                got,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::ExpectedArguments {
                expected,
                got,
                mut trace,
            }) => Err(Error::ExpectedArguments {
                expected,
                got,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::UnexpectedType {
                expected,
                got,
                mut trace,
            }) => Err(Error::UnexpectedType {
                expected,
                got,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::IdNotFound { id, mut trace }) => Err(Error::IdNotFound {
                id,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::FieldNotFound { field, mut trace }) => Err(Error::FieldNotFound {
                field,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::FunctionNotFound {
                function,
                mut trace,
            }) => Err(Error::FunctionNotFound {
                function,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::IndexOutOfBounds { mut trace }) => Err(Error::IndexOutOfBounds {
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::KeyNotFound { mut trace }) => Err(Error::KeyNotFound {
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
            Err(Error::Custom { error, mut trace }) => Err(Error::Custom {
                error,
                trace: {
                    trace.push_front(node);
                    trace
                },
            }),
        }
    }
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("expected callable, got `{got}`")]
    ExpectedCallable { got: Value, trace: VecDeque<AST> },

    #[error("expected num, got `{got}`")]
    ExpectedNum { got: Value, trace: VecDeque<AST> },

    #[error("expected cmp, got `{got}`")]
    ExpectedCmp { got: Value, trace: VecDeque<AST> },

    #[error("expected concat, got `{got}`")]
    ExpectedConcat { got: Value, trace: VecDeque<AST> },

    #[error("expected indexable, got `{got}`")]
    ExpectedIndexable { got: Value, trace: VecDeque<AST> },

    #[error("expected keyable, got `{got}`")]
    ExpectedKeyable { got: Value, trace: VecDeque<AST> },

    #[error("expected {expected} arguments, got {got} arguments")]
    ExpectedArguments {
        expected: usize,
        got: usize,
        trace: VecDeque<AST>,
    },

    #[error("expected {expected}, got `{got}`")]
    UnexpectedType {
        expected: Type,
        got: Value,
        trace: VecDeque<AST>,
    },

    #[error("id not found `{id}`")]
    IdNotFound { id: Id, trace: VecDeque<AST> },

    #[error("field not found `{field}`")]
    FieldNotFound { field: String, trace: VecDeque<AST> },

    #[error("function not found `{function}`")]
    FunctionNotFound {
        function: Function,
        trace: VecDeque<AST>,
    },

    #[error("index out of bounds")]
    IndexOutOfBounds { trace: VecDeque<AST> },

    #[error("key not found")]
    KeyNotFound { trace: VecDeque<AST> },

    #[error("{error}")]
    Custom { error: String, trace: VecDeque<AST> },
}

impl Error {
    pub fn trace(&self) -> &VecDeque<AST> {
        match self {
            Self::ExpectedCallable { trace, .. } => trace,
            Self::ExpectedNum { trace, .. } => trace,
            Self::ExpectedCmp { trace, .. } => trace,
            Self::ExpectedConcat { trace, .. } => trace,
            Self::ExpectedIndexable { trace, .. } => trace,
            Self::ExpectedKeyable { trace, .. } => trace,
            Self::ExpectedArguments { trace, .. } => trace,
            Self::UnexpectedType { trace, .. } => trace,
            Self::IdNotFound { trace, .. } => trace,
            Self::FieldNotFound { trace, .. } => trace,
            Self::FunctionNotFound { trace, .. } => trace,
            Self::IndexOutOfBounds { trace, .. } => trace,
            Self::KeyNotFound { trace, .. } => trace,
            Self::Custom { trace, .. } => trace,
        }
    }
}

pub fn sprint_trace(trace: &VecDeque<AST>) -> String {
    sprint_trace_with_ident(trace, "".to_string())
}

pub fn sprint_trace_with_ident(trace: &VecDeque<AST>, indent: impl Into<String>) -> String {
    sprint_trace_with_ident_and_sp(trace, indent.into(), 0)
}

fn sprint_trace_with_ident_and_sp(trace: &VecDeque<AST>, indent: String, i: usize) -> String {
    if i >= trace.len() {
        return String::new();
    }
    let mut string = format!("{}{}\n", indent, trace[i]);
    string += &sprint_trace_with_ident_and_sp(trace, indent + "  ", i + 1);
    string
}
