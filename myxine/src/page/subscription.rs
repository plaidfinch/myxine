use std::collections::{HashMap, HashSet};
use hyper_usse::EventBuilder;
use std::sync::{Arc, Weak};
use serde::{Serialize, Deserialize};
use serde_json::Value;
use hyper::Body;
use futures::future;

use super::sse;

/// An incoming event sent from the browser, intended to be forwarded directly
/// to listeners. While there is more structure here than merely a triple of
/// JSON values, we don't bother to parse it because the client will be parsing
/// it again, so this will be a waste of resources.
#[derive(Debug, Clone, Deserialize)]
pub struct Event {
    pub event: String,
    pub id: Value,
    pub data: Value,
}

/// A subscription to events is either a `Universal` subscription to all events,
/// or a `Specific` set of events to which the client wishes to subscribe.
#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum AggregateSubscription<'a> {
    Specific(HashSet<&'a String>),
    Universal,
}

impl<'a> AggregateSubscription<'a> {
    /// Make an empty aggregate subscription (useful when specifying that a page
    /// should disconnect all open subscribers).
    pub fn empty() -> AggregateSubscription<'a> {
        AggregateSubscription::Specific(HashSet::new())
    }
}

/// The maximum number of messages to buffer before blocking a send. This means
/// a client can send a burst of up to this number of UI events before it
/// experiences backpressure. It usually makes sense for this to be much higher
/// than the buffer size for page content, because page content is likely to be
/// larger and less "bursty."
const EVENT_BUFFER_SIZE: usize = 10_000;

#[derive(Debug)]
pub struct Subscribers {
    servers: Vec<Arc<sse::BufferedServer>>,
    specific: HashMap<String, Vec<Sink>>,
    universal: Vec<Sink>,
}

#[derive(Debug, Clone)]
struct Sink {
    server: Weak<sse::BufferedServer>,
}

impl Subscribers {
    /// Make a new empty set of subscribers.
    pub fn new() -> Subscribers {
        Subscribers {
            specific: HashMap::new(),
            universal: Vec::new(),
            servers: Vec::new(),
        }
    }

    /// Returns `true` if there are no subscribers to any events in this set of
    /// subscribers, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.specific.is_empty() && self.servers.is_empty()
    }

    /// Add a new subscription to this set of subscribers, returning a streaming
    /// body to be sent to the subscribing client.
    pub async fn add_subscriber(
        &mut self,
        subscription: Subscription,
    ) -> (AggregateSubscription<'_>, Body) {
        // Create a new single-client SSE server (new clients will never be added
        // after this, because each event subscription is potentially unique).
        let server = sse::BufferedServer::new(EVENT_BUFFER_SIZE).await;
        let (sender, body) = Body::channel();
        server.add_client(sender).await;
        let server = Arc::new(server);

        // Add a reference to the server, with the appropriate property filter,
        // to each place corresponding to its desired subscription.
        match subscription {
            Subscription::Specific(events) => {
                for event in events {
                    let existing_sinks =
                        self.specific.entry(event).or_insert_with(Vec::new);
                    existing_sinks.push(Sink{server: Arc::downgrade(&server)});
                }
            },
            Subscription::Universal => {
                self.universal.push(Sink{server: Arc::downgrade(&server)});
            },
        }

        // Add the server to the top-level list of server references
        self.servers.push(server);

        // Return the body, for sending to whoever subscribed
        (self.total_subscription(), body)
    }

    /// Send an event to all subscribers to that event, giving each subscriber
    /// only those fields of the event which that subscriber cares about. If the
    /// list of subscribers has changed (that is, by client disconnection),
    /// returns the union of all now-current subscriptions.
    pub async fn send_event<'a>(&'a mut self, event: &str, id: &Value, data: &Value) -> Option<AggregateSubscription<'a>> {
        let mut subscription_changed = false;

        // Sent the event to any specifically interested clients:
        if let Some(specific) = self.specific.get_mut(event) {
            let previous_len = specific.len();
            send_to_all(specific, event, id, data).await;
            if specific.len() != previous_len {
                subscription_changed = true
            }
            if specific.len() == 0 {
                self.specific.remove(event);
            }
        }

        // Send the event to all universally interested clients:
        let previous_len = self.universal.len();
        send_to_all(&mut self.universal, event, id, data).await;
        if self.universal.len() != previous_len {
            subscription_changed = true
        }

        // Return a new aggregate subscription if things have changed
        if subscription_changed {
            Some(self.total_subscription())
        } else {
            None
        }
    }

    /// Send a heartbeat message to all subscribers, returning a new
    /// Subscription representing the union of all subscriptions, if there have
    /// been any noticed changes, or `None` if every client is still connected.
    pub async fn send_heartbeat(&mut self) -> Option<AggregateSubscription<'_>> {
        // Send a heartbeat message to every server (but only once each!)
        let mut sent = future::join_all({
            self.servers.iter_mut().map(|server| {
                async move {
                    let remaining = server.send_heartbeat().await.await;
                    assert!(remaining <= 1, "Subscriber SSE exceeds 1 client");
                    1 == remaining
                }
            })
        }).await.into_iter();

        // Prune all servers for which heartbeat failed (since there's exactly
        // one Arc for each one of them, the corresponding Weaks within the
        // specific will now return None on upgrade)
        self.servers.retain(|_| sent.next().unwrap());

        // Prune all specific subscribers corresponding to the dropped servers
        let mut subscription_changed = false;
        self.specific.retain(|_, sinks| {
            let previous_len = sinks.len();
            sinks.retain(|Sink{server, ..}| {
                if server.upgrade().is_none() {
                    subscription_changed = true;
                    false // prune this servers
                } else {
                    true // keep this server
                }
            });
            sinks.shrink_to_fit();
            previous_len == sinks.len()
        });

        // Prune all universal subscribers corresponding to the dropped servers
        self.universal.retain(|Sink{server, ..}| {
            if server.upgrade().is_none() {
                subscription_changed = true;
                false // prune this server
            } else {
                true // keep this server
            }
        });

        // Report the changed subscription
        if subscription_changed {
            Some(self.total_subscription())
        } else {
            None
        }
    }

    /// Calculate the union of all subscriptions currently active
    pub fn total_subscription(&self) -> AggregateSubscription<'_> {
        if self.universal.is_empty() {
            let events = self.specific.keys().collect();
            AggregateSubscription::Specific(events)
        } else {
            AggregateSubscription::Universal
        }
    }
}

/// Send an event to all the sinks in the given `Vec`, pruning the `Vec` to
/// remove all those sinks which have become disconnected.
async fn send_to_all<'a>(
    sinks: &'a mut Vec<Sink>,
    event: &'a str,
    id: &'a Value,
    data: &'a Value,
) {
    // Serialize the fields to JSON
    let data = serde_json::to_string(data)
        .expect("Serializing to a string shouldn't fail");

    // Serialize the target path to JSON
    let id = serde_json::to_string(id)
        .expect("Serializing to a string shouldn't fail");

    // The collection of futures for sending the event:
    let send_futures = sinks.iter_mut().map(move |sink| {
        // Build a text/event-stream message to send to subscriber
        let message = EventBuilder::new(&data).id(&id).event_type(event).build();
        // Make a future for sending the message to the subscriber
        async move {
            if let Some(server) = sink.server.upgrade() {
                let remaining =
                    server.send_to_clients(message).await.await;
                assert!(remaining <= 1, "Subscriber SSE exceeds 1 client");
                1 == remaining
            } else {
                false
            }
        }
    });

    // Actually send the event to everyone
    let mut sent = future::join_all(send_futures).await.into_iter();

    // Remove all sinks that failed to send (client disconnected)
    sinks.retain(|_| sent.next().unwrap());
}
