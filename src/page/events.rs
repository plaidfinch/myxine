use std::collections::{HashMap, HashSet};
use hyper_usse::EventBuilder;
use std::sync::{Arc, Weak};
use serde::{Serialize, Deserialize};
use serde_json::Value;
use hyper::Body;
use futures::future;
use std::string::ToString;
use uuid::Uuid;

use super::sse;

// Exterior interface is opaque type `Subscription` which can only be
// constructed by deserializing it (i.e. from JSON)...

#[derive(Debug, Clone, Deserialize)]
pub struct Subscription(
    HashMap<String, HashSet<String>>
);

#[derive(Debug, Clone, Serialize)]
pub struct AggregateSubscription<'a>(
    HashMap<&'a String, HashSet<&'a String>>
);

impl<'a> AggregateSubscription<'a> {
    /// Make an empty aggregate subscription (useful when specifying that a page
    /// should disconnect all open subscribers).
    pub fn empty() -> AggregateSubscription<'a> {
        AggregateSubscription(HashMap::new())
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
    servers: HashMap<Uuid, Arc<sse::BufferedServer>>,
    routes: HashMap<String, HashMap<String, Vec<Sink>>>,
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
            servers: HashMap::new(),
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
        old_uuid: Option<Uuid>,
        subscription: Subscription,
    ) -> (AggregateSubscription<'_>, Option<(Uuid, Body)>) {
        // Either use the given UUID, or make a new one
        if let Some(old_uuid) = old_uuid {
            if !self.servers.contains_key(&old_uuid) {
                // If the user is trying to re-subscribe to a stream that
                // doesn't exist, that's an error and we should report it as
                // such. This prevents them from, e.g. hard-coding stream IDs
                // into programs and leading to subtle bugs where programs steal
                // each others' streams using ?resubscribe.
                return (self.total_subscription(), None)
            } else {
                // If the old stream *did* exist, we kill it and make a new one
                // with a new UUID. This linearity makes it so that the user has
                // to *explicitly* share a new ?resubscribe URI if they really
                // really really want to have threads stealing from each other.
                self.servers.remove(&old_uuid);
            }
        }
        let new_uuid = Uuid::new_v4();
        // Create a new single-client SSE server (new clients will never be added
        // after this, because each event subscription is potentially unique).
        let server = sse::BufferedServer::new(EVENT_BUFFER_SIZE).await;
        let (sender, body) = Body::channel();
        server.add_client(sender).await;
        let server = Arc::new(server);
        // Add a reference to the server, with the appropriate property filter,
        // to each place corresponding to its desired subscription.
        for (target, events) in subscription.0 {
            let existing_events =
                self.routes.entry(target).or_insert_with(HashMap::new);
            for event in events {
                let existing_sinks =
                    existing_events.entry(event).or_insert_with(Vec::new);
                existing_sinks.push(Sink{server: Arc::downgrade(&server)});
            }
        }
        // Add the server to the top-level list of server references
        self.servers.insert(new_uuid, server);
        // Return the body, for sending to whoever subscribed
        (self.total_subscription(), Some((new_uuid, body)))
    }

    /// Send an event to all subscribers to that event, giving each subscriber
    /// only those fields of the event which that subscriber cares about. If the
    /// list of subscribers has changed (that is, by client disconnection),
    /// returns the union of all now-current subscriptions.
    pub async fn send_event<'a>(&'a mut self,
                                event_type: &str,
                                event_target: &str,
                                _event_id: &str, // We don't use this right now,
                                // since it's more helpful to the user to hand
                                // them the original query they specified, and
                                // place the specific id in .currentTarget.id.
                                event_data: &HashMap<String, Value>
    ) -> Option<AggregateSubscription<'a>> {
        let mut subscription_changed = false;
        if let Some(sinks) =
            self.routes.get_mut(event_target).and_then(|m| m.get_mut(event_type)) {
                let mut sent = future::join_all(sinks.iter_mut().map(|sink| {
                    // Serialize the fields to JSON
                    let data = serde_json::to_string(&event_data)
                        .expect("Serializing fields to a string shouldn't fail");
                    // Build a text/event-stream message to send to subscriber
                    let message =
                        EventBuilder::new(&data)
                        .id(&event_target.to_string())
                        .event_type(event_type)
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
            }
        // Prune the routes to remove empty hashmaps
        if let Some(events) = self.routes.get_mut(event_target) {
            if let Some(sinks) = events.get_mut(event_type) {
                if sinks.is_empty() {
                    events.remove(event_type);
                    subscription_changed = true;
                }
            }
            if events.is_empty() {
                self.routes.remove(event_target);
                subscription_changed = true;
            }
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
        let mut sent = future::join_all(self.servers.iter_mut().map(|(_, server)| {
            async move {
                let remaining = server.send_heartbeat().await.await;
                assert!(remaining <= 1, "Subscriber SSE exceeds 1 client");
                1 == remaining
            }
        })).await.into_iter();
        // Prune all servers for which heartbeat failed (since there's exactly
        // one Arc for each one of them, the corresponding Weaks within the
        // routes will now return None on upgrade)
        self.servers.retain(|_, _| sent.next().unwrap());
        // Prune all routes corresponding to the dropped servers
        let mut subscription_changed = false;
        self.routes.retain(|_, events| {
            let previous_len = events.len();
            events.retain(|_, sinks| {
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
            events.shrink_to_fit();
            previous_len == events.len()
        });
        if subscription_changed {
            Some(self.total_subscription())
        } else {
            None
        }
    }

    /// Calculate the union of all subscriptions currently active
    pub fn total_subscription(&self) -> AggregateSubscription<'_> {
        let subscription =
            self.routes.iter().map(|(path, events)| {
                (path, events.keys().collect())
            }).collect();
        AggregateSubscription(subscription)
    }
}
