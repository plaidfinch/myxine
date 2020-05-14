use uuid::Uuid;
use std::fmt;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Unique(Uuid);

impl Unique {
    pub fn new() -> Unique {
        Unique(Uuid::new_v4())
    }

    pub fn parse_str(input: &str) -> Option<Unique> {
        let uuid = Uuid::parse_str(input).ok()?;
        Some(Unique(uuid))
    }
}

impl fmt::Display for Unique {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        let Unique(uuid) = self;
        write!(w, "{}", uuid.to_simple_ref())
    }
}
