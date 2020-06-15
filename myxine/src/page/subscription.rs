use std::collections::HashSet;
use serde::{Serialize, Deserialize};
use serde_json::Value;
use hyper::Body;
use tokio::sync::oneshot;
use futures::Future;

use super::sse::BroadcastBody;

/// An incoming event sent from the browser, intended to be forwarded directly
/// to listeners. While there is more structure here than merely a triple of
/// JSON values, we don't bother to parse it because the client will be parsing
/// it again, so this will be a waste of resources.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Event {
    pub event: String,
    pub targets: Value,
    pub properties: Value,
}

/// A subscription to events is either a `Universal` subscription to all events,
/// or a `Specific` set of events to which the client wishes to subscribe.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Subscription {
    Specific(HashSet<String>),
    Universal,
}

impl Subscription {
    /// Make a subscription from a collection of events.
    pub fn from_events(events: impl Into<HashSet<String>>) -> Self {
        Subscription::Specific(events.into())
    }

    /// Make a universal subscription to all events.
    pub fn universal() -> Self {
        Subscription::Universal
    }

    /// Tests whether a given event is a member of this subscription.
    pub fn matches_event(&self, event: &str) -> bool {
        match self {
            Subscription::Universal => true,
            Subscription::Specific(set) => set.contains(event),
        }
    }
}

/// The maximum number of messages to buffer before dropping a message (i.e. the
/// maximum a client can lag in listening for subscribed events).
const EVENT_BUFFER_SIZE: usize = 10_000;

#[derive(Debug)]
pub struct Subscribers {
    sinks: Vec<Sink>,
}

#[derive(Debug)]
pub enum SinkSender {
    Persistent(BroadcastBody),
    Once {
        sender: oneshot::Sender<(u64, hyper::body::Body)>,
        after: u64,
    },
}

#[derive(Debug)]
struct Sink {
    subscription: Subscription,
    sender: SinkSender,
}

impl Subscribers {
    /// Make a new empty set of subscribers.
    pub fn new() -> Subscribers {
        Subscribers {
            sinks: Vec::new(),
        }
    }

    /// Returns `true` if there are no subscribers to any events in this set of
    /// subscribers, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.sinks.is_empty()
    }

    /// Add a persistent subscriber for the given subscription. The returned
    /// `Body` will stream newline-separated events, and will not terminate
    /// until the consumer disconnects or the server exits.
    pub fn add_subscriber(
        &mut self, subscription: Subscription,
    ) -> Body {
        // Create a new single-client SSE server (new clients will never be added
        // after this, because each event subscription is potentially unique).
        let server = BroadcastBody::new(EVENT_BUFFER_SIZE);
        let body = server.body();
        let sender = SinkSender::Persistent(server);

        // Insert the server into the sinks map
        self.sinks.push(Sink{sender, subscription});

        // Return the body, for sending to whoever subscribed
        body
    }

    /// Add a one-off subscriber which gives the moment it was fulfilled, as
    /// well as the body of the event to which it corresponds. This `Body` will
    /// be a single valid JSON string.
    pub fn add_one_off(
        &mut self, subscription: Subscription,
        after: u64,
    ) -> impl Future<Output = (u64, hyper::body::Body)> {
        let (sender, receiver) = oneshot::channel();
        self.sinks.push(Sink{sender: SinkSender::Once{sender, after}, subscription});
        async move {
            receiver.await.expect("Receivers for one-off subscriptions shouldn't be dropped")
        }
    }

    /// Send an event to all subscribers to that event, giving each subscriber
    /// only those fields of the event which that subscriber cares about. If the
    /// list of subscribers has changed (that is, by client disconnection),
    /// returns the union of all now-current subscriptions.
    pub fn send_event<'a>(&'a mut self, moment: u64, event: &Event) {
        let message = serde_json::to_string(&event)
            .expect("Serializing to a string shouldn't fail");
        let mut i = 0;
        loop {
            if i >= self.sinks.len() {
                break;
            }
            if self.sinks[i].subscription.matches_event(&event.event) {
                let Sink{sender, subscription} = self.sinks.swap_remove(i);
                match sender {
                    SinkSender::Persistent(server)
                        // NOTE: this is an if-guard, not an expression, so it
                        // falls through to the default case if there are still
                        // clients after the message is sent
                        if 0 == server.send((message.clone() + "\n").into()) => {
                            // if there are no more subscribers to a persistent
                            // sink, remove it from the list of sinks
                        },
                    SinkSender::Once{sender, after} if moment >= after => {
                        // if the current moment is later than or equal to the
                        // time this one-off sink is waiting for, dispatch it
                        // and remove it from the list of sinks
                        sender.send((moment, message.clone().into())).unwrap_or(());
                    },
                    sender => {
                        // otherwise, re-insert the sink and move forward, by
                        // pushing the current sink onto the end of the vector
                        // and swapping it back into place, then incrementing
                        // the index by one
                        self.sinks.push(Sink{sender, subscription});
                        let end = self.sinks.len() - 1;
                        self.sinks.swap(i, end);
                        i += 1;  // move on to the next element
                    },
                }
            } else {
                i += 1;  // move onto the next element
            }
        }

        // Shrink down the sinks so we don't bloat memory
        self.sinks.shrink_to_fit();
    }
}
