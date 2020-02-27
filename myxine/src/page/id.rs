use uuid::Uuid;
use std::cmp::Ordering;
use std::sync::atomic::{self, AtomicU64};
use std::fmt;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Id<Epoch>(Epoch, Uuid);

impl<E: Epoch> Id<E> {
    pub fn new(e: E) -> Id<E> {
        Id(e, Uuid::new_v4())
    }

    pub fn parse_str(input: &str) -> Option<Id<E>> {
        match input.splitn(2, '-').collect::<Vec<_>>().as_slice() {
            [epoch_part, uuid_part] => {
                let uuid = Uuid::parse_str(uuid_part).ok()?;
                Some(Id(Epoch::parse_epoch(epoch_part)?, uuid))
            },
            [uuid_part] => {
                let uuid = Uuid::parse_str(uuid_part).ok()?;
                Some(Id(Epoch::parse_epoch("")?, uuid))
            }
            _ => None,
        }
    }
}

impl<E: Epoch> PartialOrd for Id<E> {
    fn partial_cmp(&self, Id(other_epoch, other_uuid): &Id<E>) -> Option<std::cmp::Ordering> {
        let Id(this_epoch, this_uuid) = self;
        match this_epoch.cmp(other_epoch) {
            Ordering::Equal if this_uuid != other_uuid => None,
            ordering => Some(ordering),
        }
    }
}

impl<E: Epoch> fmt::Display for Id<E> {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        let Id(e, uuid) = self;
        e.write_epoch(w)?;
        write!(w, "-")?;
        write!(w, "{}", uuid.to_simple_ref())
    }
}

pub trait Epoch where Self: Ord + Sized + Clone {
    fn parse_epoch(input: &str) -> Option<Self>;
    fn write_epoch(&self, w: &mut fmt::Formatter) -> fmt::Result;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Global;

impl Epoch for Global {
    fn parse_epoch(input: &str) -> Option<Global> {
        if input == "global" {
            Some(Global)
        } else {
            None
        }
    }

    fn write_epoch(&self, w: &mut fmt::Formatter) -> fmt::Result {
        write!(w, "global")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Frame {
    frame_created: u64,
}

impl Frame {
    pub fn new() -> Frame {
        static FRESH_FRAME: AtomicU64 = AtomicU64::new(0);
        Frame {
            frame_created: FRESH_FRAME.fetch_add(1, atomic::Ordering::SeqCst)
        }
    }
}

impl Epoch for Frame {
    fn parse_epoch(input: &str) -> Option<Frame> {
        if let &["frame", number] =
            input.split('!').collect::<Vec<_>>().as_slice()
        {
            Some(Frame { frame_created: number.parse().ok()? })
        } else {
            None
        }
    }

    fn write_epoch(&self, w: &mut fmt::Formatter) -> fmt::Result {
        write!(w, "frame!{}", self.frame_created)
    }
}
