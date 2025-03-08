use regex::{Error, Regex};
use serde::Deserializer;
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};

/// A wrapper around `regex::Regex` with custom serialization, deserialization,
/// equality, hashing, and display implementations.
///
/// This struct stores both the compiled `regex::Regex` and its original pattern string.
/// It provides custom implementations for Serde serialization/deserialization,
/// `PartialEq`, `Eq`, `Hash`, and `Display` to make it easier to work with
/// regular expressions in various contexts, especially when serialization or
/// hashing is required.
#[derive(Clone, serde::Serialize, serde::Deserialize, Debug)]
pub struct WrapperRegex {
    /// The original pattern string used to create the regex. This is used for Equality Checks
    pattern: String,
    /// The compiled regular expression.
    #[serde(with = "serde_regex")]
    regex: Regex,
}

impl WrapperRegex {
    /// Creates a new `WrapperRegex` from the given pattern string.
    ///
    /// # Arguments
    ///
    /// * `pattern` - The regular expression pattern string.
    ///
    /// # Returns
    ///
    /// A `Result` containing the `WrapperRegex` on success, or a `regex::Error`
    /// if the pattern is invalid.
    pub fn new(pattern: String) -> Result<WrapperRegex, Error> {
        match Regex::new(&pattern) {
            Ok(regex) => Ok(WrapperRegex { pattern, regex }),
            Err(e) => Err(e),
        }
    }

    /// Returns a clone of the underlying `Regex`.
    /// This is relative inexpensive, since it doesn't invoke a recompilation
    pub fn get_regex(&self) -> Regex {
        self.regex.clone()
    }
}

impl PartialEq for WrapperRegex {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern
    }
}

impl Eq for WrapperRegex {}

impl Hash for WrapperRegex {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pattern.hash(state);
    }
}

/// Module for custom Serde serialization and deserialization of `regex::Regex`.
mod serde_regex {
    use super::*;
    use serde::{Deserialize, Serializer};

    /// Serializes a `Regex` into a string that contains the pattern.
    pub fn serialize<S>(regex: &Regex, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(regex.as_str())
    }

    /// Deserializes a `Regex` from a string.
    pub fn deserialize<'de, D>(deserializer: D) -> Result<Regex, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Regex::new(&s).map_err(serde::de::Error::custom)
    }
}

impl std::fmt::Display for WrapperRegex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "pattern:{}", self.pattern)
    }
}
