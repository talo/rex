use std::fmt::{self, Display, Formatter};

use rex_ast::id::Id;
use rpds::HashTrieMapSync;

use crate::value::Value;

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Context {
    vars: HashTrieMapSync<Id, Value>,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            vars: HashTrieMapSync::new_sync(),
        }
    }

    pub fn insert(&mut self, k: Id, v: Value) {
        self.vars.insert_mut(k, v);
    }

    pub fn extend(&mut self, rhs: &Context) {
        for (k, v) in rhs.vars.iter() {
            self.vars.insert_mut(*k, v.clone());
        }
    }

    pub fn get(&self, k: &Id) -> Option<Value> {
        self.vars.get(k).cloned()
    }
}

impl Display for Context {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, (k, v)) in self.vars.iter().enumerate() {
            k.fmt(f)?;
            "←".fmt(f)?;
            v.fmt(f)?;
            if i + 1 < self.vars.size() {
                "; ".fmt(f)?;
            }
        }
        Ok(())
    }
}
