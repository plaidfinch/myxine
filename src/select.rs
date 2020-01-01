use std::collections::{HashMap, HashSet};
use serde::{Serialize, Deserialize};
use serde_json::Value;
use std::mem;

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Selectors {
    Nothing,
    Path(String),
    Set(Vec<Selectors>),
    Map(HashMap<String, Selectors>),
}

#[derive(Debug, Clone, Serialize)]
pub struct CanonSelectors<'a>(HashMap<&'a str, CanonSelectors<'a>>);

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum Selected<'a> {
    Nothing,
    Map(HashMap<&'a str, Selected<'a>>),
    Value(&'a Value),
}

fn get_path<'a, 'b>(path: &'a str, mut value: &'a Value) -> &'a Value {
    let segments = path.split('.');
    for field in segments {
        value = value.get(field);
    }
    value
}

impl Selectors {
    pub fn filter<'a>(&'a self, value: &'a Value) -> Selected<'a> {
        match self {
            Selectors::Nothing => Selected::Nothing,
            Selectors::Path(path) => {
                let result = HashMap::new();
                result.insert(path, get_path(path, value));
                result
            },
            Selectors::Set(list) => {
                for selector in list {

                }
            },
            Selectors::Map(map) => {
                let result = HashMap::with_capacity(map.len());
                for (path, selectors) in map {
                    result.insert(path, selectors.filter(get_path(path, value)));
                }
                result
            }
        }
    }
}

impl Selectors {
    pub fn canonicalize<'a>(&'a self) -> CanonSelectors<'a> {
        match self {
            Selectors::Nothing => CanonSelectors::new(),
            Selectors::Path(key) => {
                let mut canon = HashMap::new();
                canon.insert(key, CanonSelectors::new());
                CanonSelectors(canon)
            },
            Selectors::Set(list) => {
                let canons = list.into_iter().map(Selectors::canonicalize);
                CanonSelectors::unions(canons)
            },
            Selectors::Map(map) => {
                let mut canon = HashMap::with_capacity(map.len());
                for (key, selectors) in map.into_iter() {
                    canon.insert(key, selectors.canonicalize());
                }
                CanonSelectors(canon)
            },
        }
    }
}

impl<'a> CanonSelectors<'a> {
    fn new() -> CanonSelectors<'a> {
        CanonSelectors(HashMap::new())
    }

    pub fn union(&mut self, CanonSelectors(mut other): CanonSelectors<'a>) {
        // We'll always iterate over whichever is shorter, call it `other` now
        if other.len() > self.0.len() {
            mem::swap(&mut self.0, &mut other);
        }
        for (key, selectors) in other.into_iter() {
            self.0.entry(key).or_insert_with(|| CanonSelectors::new())
                .union(selectors)
        }
    }

    pub fn unions(selectors: impl Iterator<Item = CanonSelectors<'a>>) -> CanonSelectors<'a> {
        selectors.fold(CanonSelectors::new(),
                       |mut s, t| { s.union(t); s })
    }
}
