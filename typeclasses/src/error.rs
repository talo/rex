use std::fmt;
use std::rc::Rc;
use crate::types::Type;

#[derive(Eq, PartialEq, Debug)]
pub enum TypeError {
    MergeFails,
    TypesDoNotUnify(Rc<Type>, Rc<Type>),
    TypesDoNotMatch,
    OccursCheckFails,
    KindsDoNotMatch,
    ClassesDiffer,
    UnboundIdentifier(String),
    ClassAlreadyDefined(String),
    SuperclassNotDefined(String),
    NoClassForInstance(String),
    OverlappingInstance,
}

impl std::error::Error for TypeError {
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            TypeError::TypesDoNotUnify(a, b) => write!(f, "Types do not unify: {} and {}", a, b),
            _ => fmt::Debug::fmt(self, f)
        }
    }
}
