use std::fmt;

#[derive(Eq, PartialEq, Debug)]
pub enum TypeError {
    MergeFails,
    TypesDoNotUnify,
    TypesDoNotMatch,
    OccursCheckFails,
    KindsDoNotMatch,
    ClassesDiffer,
    UnboundIdentifier(String),
}

impl std::error::Error for TypeError {
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt::Debug::fmt(self, f)
    }
}
