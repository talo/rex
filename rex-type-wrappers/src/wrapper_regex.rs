use std::fmt::Formatter;
use std::hash::{Hash, Hasher};
use regex::{Error, Regex};
use serde::{Deserializer};

#[derive(Clone, serde::Serialize, serde::Deserialize, Debug)]
pub struct WrapperRegex {
    pattern :String ,
    #[serde(with ="serde_regex")]
    regex : Regex,
}

impl WrapperRegex {
    pub fn new(pattern :String) -> Result<WrapperRegex, Error> {
        match Regex::new(&pattern) {
            Ok(regex) => Ok(WrapperRegex {
                pattern,
                regex
            }),
            Err(e) => Err(e)
        }
    }

    pub fn get_regex(&self) -> Regex {
         self.regex.clone()
    }
}

impl PartialEq for WrapperRegex {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern
    }
}

impl Eq for WrapperRegex {

}

impl Hash for WrapperRegex{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pattern.hash (state);
    }
}

mod serde_regex {
    use serde::{Deserialize, Serializer};
    use super::*;

    pub fn serialize<S>(regex: &Regex, serializer: S) -> Result<S::Ok, S::Error>
    where
        S:  Serializer,
    {
        serializer.serialize_str(regex.as_str())
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Regex, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Regex::new(&s).map_err(serde::de::Error::custom)
    }
}

impl std::fmt::Display for WrapperRegex{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f , "pattern:{}", self.pattern)
    }
}

