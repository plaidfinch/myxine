use serde::{Serialize, Deserialize};
use std::hash::Hash;
use itertools::Itertools;
use std::convert::TryFrom;
use std::string::ToString;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[serde(into = "String", try_from = "String")]
pub enum Path {
    Absolute(AbsolutePath),
    Relative(RelativePath),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[serde(into = "String", try_from = "String")]
pub enum AbsolutePath {
    Id(String),
    Absolute(Vec<String>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[serde(into = "String", try_from = "String")]
pub struct RelativePath(Vec<String>);

impl From<AbsolutePath> for Path {
    fn from(path: AbsolutePath) -> Path {
        Path::Absolute(path)
    }
}

impl From<RelativePath> for Path {
    fn from(path: RelativePath) -> Path {
        Path::Relative(path)
    }
}

impl ToString for Path {
    fn to_string(&self) -> String {
        self.clone().into()
    }
}

impl ToString for AbsolutePath {
    fn to_string(&self) -> String {
        self.clone().into()
    }
}

impl ToString for RelativePath {
    fn to_string(&self) -> String {
        self.clone().into()
    }
}

impl Into<String> for Path {
    fn into(self) -> String {
        match self {
            Path::Absolute(path) => path.into(),
            Path::Relative(path) => path.into(),
        }
    }
}

impl Into<String> for RelativePath {
    fn into(self) -> String {
        format!(".{}", self.0.into_iter().format("."))
    }
}

impl Into<String> for AbsolutePath {
    fn into(self) -> String {
        match self {
            AbsolutePath::Id(id) =>
                format!("#{}", id),
            AbsolutePath::Absolute(segments) =>
                format!("{}", segments.into_iter().format(".")),
        }
    }
}

impl TryFrom<String> for Path {
    type Error = String;
    fn try_from(string: String) -> Result<Path, String> {
        Ok(match AbsolutePath::try_from(string.clone()) {
            Err(_) => Path::Relative(RelativePath::try_from(string)?),
            Ok(path) => Path::Absolute(path),
        })
    }
}

impl TryFrom<String> for AbsolutePath {
    type Error = String;
    fn try_from(string: String) -> Result<AbsolutePath, String> {
        Ok(if string.len() == 0 {
            AbsolutePath::Absolute(Vec::new())
        } else if let (".", _) = string.split_at(1) {
            return Err(format!("Invalid absolute path: {}", string))
        } else if let ("#", id) = string.split_at(1) {
            AbsolutePath::Id(id.to_string())
        } else {
            let segments: Vec<String> =
                string.split('.').map(String::from).collect();
            for s in &segments {
                if s.len() == 0 {
                    return Err(format!("Invalid absolute path: {}", string))
                }
            }
            AbsolutePath::Absolute(segments)
        })
    }
}

impl TryFrom<String> for RelativePath {
    type Error = String;
    fn try_from(string: String) -> Result<RelativePath, String> {
        Ok(if let (".", path) = string.split_at(1) {
            let segments: Vec<String> =
                path.split('.').map(String::from).collect();
            for s in &segments {
                if s.len() == 0 {
                    return Err(format!("Invalid relative path: {}", string))
                }
            }
            RelativePath(segments)
        } else {
            return Err(format!("Invalid relative path: {}", string))
        })
    }
}
