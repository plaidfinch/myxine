use std::collections::{HashMap, HashSet};
use hyper_usse::EventBuilder;
use std::sync::{Arc, Weak};
use serde::{Serialize, Deserialize};
use serde_json::Value;
use hyper::Body;
use futures::future;
use std::string::ToString;

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

// Exterior interface is opaque type `Subscription` which can only be
// constructed by deserializing it (i.e. from JSON)...

#[derive(Debug, Clone)]
pub struct Subscription(HashSet<String>);

impl From<Vec<String>> for Subscription {
    fn from(events: Vec<String>) -> Self {
        Subscription(events.into_iter().collect())
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct AggregateSubscription<'a>(HashSet<&'a String>);

impl<'a> AggregateSubscription<'a> {
    /// Make an empty aggregate subscription (useful when specifying that a page
    /// should disconnect all open subscribers).
    pub fn empty() -> AggregateSubscription<'a> {
        AggregateSubscription(HashSet::new())
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
    routes: HashMap<String, Vec<Sink>>,
}

#[derive(Debug, Clone)]
struct Sink {
    server: Weak<sse::BufferedServer>,
}

impl Subscribers {
    /// Make a new empty set of subscribers.
    pub fn new() -> Subscribers {
        Subscribers {
            routes: HashMap::new(),
            servers: Vec::new(),
        }
    }

    /// Returns `true` if there are no subscribers to any events in this set of
    /// subscribers, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.routes.is_empty() && self.servers.is_empty()
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
        for event in subscription.0 {
            let existing_sinks =
                self.routes.entry(event).or_insert_with(Vec::new);
            existing_sinks.push(Sink{server: Arc::downgrade(&server)});
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
        if let Some(sinks) =
            self.routes.get_mut(event) {
                let mut sent = future::join_all(sinks.iter_mut().map(|sink| {
                    // Serialize the fields to JSON
                    let data = serde_json::to_string(&data)
                        .expect("Serializing fields to a string shouldn't fail");
                    // Build a text/event-stream message to send to subscriber
                    let message =
                        EventBuilder::new(&data)
                        .id(&id.to_string())
                        .event_type(event)
                        .build();
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
                })).await.into_iter();
                // Remove all sinks that failed to send (client disconnected)
                let previous_len = sinks.len();
                sinks.retain(|_| sent.next().unwrap());
                if sinks.len() != previous_len { subscription_changed = true }
                if sinks.len() == 0 { self.routes.remove(event); }
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
        let mut sent = future::join_all(self.servers.iter_mut().map(|server| {
            async move {
                let remaining = server.send_heartbeat().await.await;
                assert!(remaining <= 1, "Subscriber SSE exceeds 1 client");
                1 == remaining
            }
        })).await.into_iter();
        // Prune all servers for which heartbeat failed (since there's exactly
        // one Arc for each one of them, the corresponding Weaks within the
        // routes will now return None on upgrade)
        self.servers.retain(|_| sent.next().unwrap());
        // Prune all routes corresponding to the dropped servers
        let mut subscription_changed = false;
        self.routes.retain(|_, sinks| {
            let previous_len = sinks.len();
            sinks.retain(|Sink{server, ..}| {
                if server.upgrade().is_none() {
                    subscription_changed = true;
                    false // prune this route
                } else {
                    true // keep this route
                }
            });
            sinks.shrink_to_fit();
            previous_len == sinks.len()
        });
        if subscription_changed {
            Some(self.total_subscription())
        } else {
            None
        }
    }

    /// Calculate the union of all subscriptions currently active
    pub fn total_subscription(&self) -> AggregateSubscription<'_> {
        let subscription = self.routes.keys().collect();
        AggregateSubscription(subscription)
    }
}
