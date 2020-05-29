use std::collections::HashSet;
use serde::Deserialize;
use serde_json::{Value, json};
use hyper::Body;
use futures::future;

use super::sse::BroadcastBody;

/// An incoming event sent from the browser, intended to be forwarded directly
/// to listeners. While there is more structure here than merely a triple of
/// JSON values, we don't bother to parse it because the client will be parsing
/// it again, so this will be a waste of resources.
#[derive(Debug, Clone, Deserialize)]
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
    Once(hyper::body::Sender),
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

    /// Add a subscriber for the given subscription, either persistent or
    /// one-off. If persistent, the returned `Body` will stream
    /// newline-separated events, and will not terminate until the consumer
    /// disconnects or the server exits. If not persistent, the returned `Body`
    /// will result in precisely one event, and then end.
    pub async fn add_subscriber(
        &mut self, subscription: Subscription, persistent: bool,
    ) -> Body {
        // Create a new single-client SSE server (new clients will never be added
        // after this, because each event subscription is potentially unique).
        let (sender, body) = if persistent {
            let server = BroadcastBody::new(EVENT_BUFFER_SIZE);
            let body = server.body();
            (SinkSender::Persistent(server), body)
        } else {
            let (sender, body) = hyper::body::Body::channel();
            (SinkSender::Once(sender), body)
        };

        // Insert the server into the sinks map
        self.sinks.push(Sink{sender, subscription});

        // Return the body, for sending to whoever subscribed
        body
    }

    /// Send an event to all subscribers to that event, giving each subscriber
    /// only those fields of the event which that subscriber cares about. If the
    /// list of subscribers has changed (that is, by client disconnection),
    /// returns the union of all now-current subscriptions.
    pub async fn send_event<'a>(&'a mut self, event: &Event) {
        // The collection of futures for sending the event Each future has
        // Output = bool, where `false` indicates that the sink should be
        // pruned.
        let send_futures = self.sinks.iter_mut().map(move |sink| {
            // Make a future for sending the message to the subscriber
            async move {
                let Event{event: event_type, properties, targets} = event;
                // Message was a normal message
                if sink.subscription.matches_event(&event_type) {
                    // Serialize the fields to JSON
                    let message = serde_json::to_string(&json!({
                        "event": event_type,
                        "properties": properties,
                        "targets": targets,
                    })).expect("Serializing to a string shouldn't fail");
                    match sink.sender {
                        SinkSender::Persistent(ref server) =>
                            0 < server.send((message + "\n").into()),
                            // we should prune if no more clients
                        SinkSender::Once(ref mut sender) => {
                            sender.send_data(message.into()).await.unwrap_or(());
                            false // we should prune the sink, it's fulfilled
                        },
                    }
                } else {
                    true // we should keep this sink, it didn't match
                }
            }
        });

        // Send all the events and remove all sinks that failed to send (client
        // disconnected), or which were one-off sinks that got fulfilled.
        let which_to_keep = future::join_all(send_futures).await;
        let mut keep = which_to_keep.iter();
        self.sinks.retain(|_| *keep.next().expect(
            "sinks lenth always matches length of futures from sending to sinks"
        ));
        
        // Shrink down the sinks so we don't bloat memory
        self.sinks.shrink_to_fit();
    }
}
