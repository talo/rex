use std::fmt::{self, Display, Formatter};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct Id(pub u64);

impl Id {
    pub fn inc(&mut self) -> Id {
        let id = self.0;
        self.0 += 1;
        Id(id)
    }
}

impl Default for Id {
    fn default() -> Self {
        Self(0)
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct IdDispenser {
    id: Id,
}

impl IdDispenser {
    pub fn new() -> IdDispenser {
        IdDispenser { id: Id::default() }
    }

    pub fn next(&mut self) -> Id {
        self.id.inc()
    }
}
