use std::fmt::{self, Display, Formatter};

#[derive(
    Clone,
    Copy,
    Debug,
    Default,
    Eq,
    Hash,
    Ord,
    PartialEq,
    PartialOrd,
    serde::Deserialize,
    serde::Serialize,
)]
#[serde(rename_all = "lowercase")]
pub struct Id(pub u64);

impl Id {
    pub fn inc(&mut self) -> Id {
        let id = self.0;
        self.0 += 1;
        Id(id)
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IdDispenser {
    id: Id,
}

impl IdDispenser {
    pub fn new() -> IdDispenser {
        IdDispenser { id: Id::default() }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Id {
        self.id.inc()
    }
}

impl Default for IdDispenser {
    fn default() -> Self {
        Self::new()
    }
}
