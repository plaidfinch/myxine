use std::collections::{HashMap, HashSet};
use hyper_usse::EventBuilder;
use std::sync::{Arc, Weak};
use tokio::sync::Mutex;
use serde::{Serialize, Deserialize};
use serde_json::Value;
use hyper::Body;
use futures::future;

use crate::select::{Selectors, CanonSelectors};

// Exterior interface is opaque type `Subscription` which can only be
// constructed by deserializing it (i.e. from JSON)...

#[derive(Debug, Clone, Deserialize)]
pub struct Subscription(HashMap<Id, HashMap<EventType, Selectors>>);

#[derive(Debug, Clone, Serialize)]
pub struct CanonSubscription<'a>(HashMap<Id, HashMap<EventType, CanonSelectors<'a>>>);

type EventType = String;
type Id = String;

#[derive(Debug)]
pub struct Subscribers {
    servers: Vec<Weak<Mutex<hyper_usse::Server>>>,
    routes: HashMap<Id, HashMap<EventType, Vec<Sink>>>,
}

#[derive(Debug, Clone)]
struct Sink {
    selectors: Selectors,
    server: Arc<Mutex<hyper_usse::Server>>,
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
        assert!(self.routes.is_empty() == self.servers.is_empty(),
                "Subscribers can't have empty routes with non-empty servers, \
                 or empty servers with non-empty routes (due to cleanup).");
        self.servers.is_empty()
    }

    /// Add a new subscription to this set of subscribers, returning a streaming
    /// body to be sent to the subscribing client.
    pub async fn add_subscriber(&mut self, subscription: Subscription) -> (Option<Subscription>, Body) {
        // If the subscription is empty, don't bother doing anything else and
        // return an empty body so the subscriber won't wait at all. This
        // ensures the invariant that subscribing servers always refer to at
        // least one event.
        if subscription.0.is_empty() {
            return (None, Body::empty())
        }
        // Create a new single-client SSE server (new clients will never be
        // added after this, because each event subscription is potentially
        // unique).
        let mut server = hyper_usse::Server::new();
        let (sender, body) = Body::channel();
        server.add_client(sender);
        let server = Arc::new(Mutex::new(server));
        self.servers.push(Arc::downgrade(&server));
        // Add a reference to the server, with the appropriate property filter,
        // to each place corresponding to its desired subscription.
        for (id, events) in subscription.0 {
            let existing_events =
                self.routes.entry(id).or_insert_with(|| HashMap::new());
            for (event, selectors) in events {
                let existing_sinks =
                    existing_events.entry(event).or_insert_with(|| Vec::new());
                existing_sinks.push(Sink{server: server.clone(), selectors});
            }
        }
        // Return the body, for sending to whoever subscribed
        (Some(self.total_subscription()), body)
    }

    /// Send an event to all subscribers to that event, giving each subscriber
    /// only those fields of the event which that subscriber cares about. If the
    /// list of subscribers has changed (that is, by client disconnection),
    /// returns the union of all now-current subscriptions.
    pub async fn send_event(&mut self,
                            event_type: &str,
                            event_id: &str,
                            event_data: &Value) -> Option<Subscription> {
        if let Some(sinks) =
            self.routes.get_mut(event_id).and_then(|m| m.get_mut(event_type)) {
                let mut sent = future::join_all(sinks.iter_mut().map(|sink| {
                    // Filter the fields of the event to those expected by this
                    // particular subscriber
                    let filtered = sink.selectors.filter(event_data);
                    // Serialize the fields to JSON
                    let data = serde_json::to_string(&filtered)
                        .expect("Serializing fields to a string shouldn't fail");
                    // Build a text/event-stream message to send to subscriber
                    let message =
                        EventBuilder::new(&data)
                        .id(event_id)
                        .event_type(event_type)
                        .build();
                    // Make a future for sending the message to the subscriber
                    async move {
                        let remaining =
                            sink.server.lock().await
                            .send_to_clients(message).await;
                        assert!(remaining <= 1, "Subscriber SSE exceeds 1 client");
                        1 == remaining
                    }
                })).await.into_iter();
                // Remove all sinks that failed to send (client disconnected)
                sinks.retain(|_| sent.next().unwrap());
            }
        // Check subscriber servers for disconnection
        let previous_subscriber_count = self.servers.len();
        self.servers.retain(|weak| weak.upgrade().is_some());
        if 0 != previous_subscriber_count - self.servers.len() {
            Some(self.total_subscription())
        } else {
            None
        }
    }

    /// Send a heartbeat message to all subscribers, returning a new
    /// Subscription representing the union of all subscriptions, if there have
    /// been any noticed changes, or `None` if every client is still connected.
    pub async fn send_heartbeat(&mut self) -> Option<Subscription> {
        let mut sent = future::join_all(self.servers.iter_mut().map(|server| {
            async move {
                if let Some(server) = server.upgrade() {
                    let remaining = server.lock().await.send_heartbeat().await;
                    assert!(remaining <= 1, "Subscriber SSE exceeds 1 client");
                    1 == remaining
                } else {
                    false
                }
            }
        })).await.into_iter();
        // Check subscriber servers for disconnection
        let previous_subscriber_count = self.servers.len();
        self.servers.retain(|_| sent.next().unwrap());
        if 0 != previous_subscriber_count - self.servers.len() {
            Some(self.total_subscription())
        } else {
            None
        }
    }

    /// Calculate the union of all subscriptions currently active
    pub fn total_subscription(&self) -> Subscription {
        let subscription =
            self.routes.iter().map(|(id, events)| {
                (id.clone(),
                 events.iter().map(|(event_type, sinks)| {
                     (event_type.clone(),
                      sinks.iter().fold(HashSet::new(), |all, Sink{field_names, ..}| {
                          field_names.iter().fold(all, |mut all, field_name| {
                              all.insert(field_name.clone());
                              all
                          })
                      }))
                 }).collect())
            }).collect();
        Subscription(subscription)
    }
}
