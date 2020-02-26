use std::collections::HashMap;
use tokio::sync::oneshot;
use futures::Future;

use super::id::{Id, Global};

/// A set of pending queries keyed by unique id, waiting to be responded to.
#[derive(Debug)]
pub struct Queries<T> {
    pending: HashMap<Id<Global>, oneshot::Sender<T>>,
}

impl<T> Queries<T> {
    /// Create a new empty set of pending queries.
    pub fn new() -> Self {
        Queries {
            pending: HashMap::new()
        }
    }

    /// Create an unfulfilled request and return its id and the future which
    /// waits on its fulfillment.
    pub fn request(&mut self) -> (Id<Global>, impl Future<Output = Option<T>>) {
        let id = Id::new(Global);
        let (sender, recv) = oneshot::channel();
        self.pending.insert(id, sender);
        (id, async { recv.await.ok() })
    }

    /// Attempt to fulfill the request of the given id, returning the given
    /// response if there's an error sending it, or if there is no request with
    /// the specified id.
    pub fn respond(&mut self, id: Id<Global>, response: T) -> Result<(), T> {
        if let Some(sender) = self.pending.remove(&id) {
            sender.send(response)
        } else {
            Err(response)
        }
    }

    /// Cancel a pending request, so that it will never be answered, and any
    /// future response will do nothing.
    pub fn cancel(&mut self, id: Id<Global>) {
        self.pending.remove(&id);
    }

    /// Test if the set of pending queries is empty.
    pub fn is_empty(&self) -> bool {
        self.pending.is_empty()
    }
}
