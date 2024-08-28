use std::fmt::Display;

use chrono::{DateTime, Utc};

use crate::span::Span;

use super::{Error, Value};

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum TraceNode {
    Root,
    ListCtor,
    LetIn,
    Assign(String),
    Lambda(Vec<String>, Vec<Value>),
    Function(String, Vec<Value>),
    Error(Error),
}

impl Display for TraceNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TraceNode::ListCtor => write!(f, "[]"),
            TraceNode::Lambda(params, args) => {
                write!(f, "((λ ",)?;
                for (i, param) in params.iter().enumerate() {
                    param.fmt(f)?;
                    if i + 1 < params.len() {
                        write!(f, " ")?;
                    }
                }
                write!(f, ") ")?;
                for (i, arg) in args.iter().enumerate() {
                    arg.fmt(f)?;
                    if i + 1 < args.len() {
                        write!(f, " ")?;
                    }
                }
                write!(f, ")")
            }
            TraceNode::Function(name, args) => {
                write!(
                    f,
                    "({} ",
                    match name.as_str() {
                        "++" => "(++)",
                        "+" => "(+)",
                        "-" => "(-)",
                        "*" => "(*)",
                        "/" => "(/)",
                        "." => "(.)",
                        x => x,
                    }
                )?;
                for (i, arg) in args.iter().enumerate() {
                    arg.fmt(f)?;
                    if i + 1 < args.len() {
                        write!(f, " ")?;
                    }
                }
                write!(f, ")")
            }
            TraceNode::Error(err) => write!(f, "Error({})", err),
            _ => Ok(()),
        }
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct Trace {
    pub step: u64,
    pub children: Vec<Trace>,
    pub node: TraceNode,
    pub span: Span,
    pub timestamp: Option<DateTime<Utc>>,
}

impl PartialEq for Trace {
    fn eq(&self, other: &Self) -> bool {
        self.step == other.step
            && self.children == other.children
            && self.node == other.node
            && self.span == other.span
    }
}

impl Trace {
    pub fn from_span(span: Span) -> Self {
        Self {
            step: 0,
            timestamp: None,
            children: Vec::new(),
            node: TraceNode::Root,
            span,
        }
    }

    pub fn step(&mut self, node: TraceNode, span: Span) -> &mut Trace {
        let n = self.children.len();
        let next_step = if n > 0 {
            unsafe { self.children.get_unchecked_mut(n - 1) }.step + 1
        } else {
            self.step + 1
        };
        self.children.push(Self {
            step: next_step,
            children: Vec::new(),
            timestamp: Some(Utc::now()),
            node,
            span,
        });
        unsafe { self.children.get_unchecked_mut(n) }
    }

    pub fn to_string_pretty(&self, indent: usize, ident_size: usize) -> String {
        let mut s = String::new();
        if !matches!(&self.node, TraceNode::Root) {
            for _ in 0..indent {
                s.push_str(&" ".repeat(ident_size));
            }
            s.push_str(&format!("{} {}\n", self.node, self.span));
        }
        for child in self.children.iter() {
            s.push_str(&child.to_string_pretty(
                indent
                    + if matches!(&self.node, TraceNode::Root) {
                        0
                    } else {
                        1
                    },
                ident_size,
            ));
        }
        s
    }
}

impl Display for Trace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_pretty(0, 2))
    }
}
