use std::collections::{HashMap, HashSet};
use hyper_usse::EventBuilder;
use std::sync::{Arc, Weak};
use tokio::sync::Mutex;
use serde::{Serialize, Deserialize};
use serde_json::Value;
use hyper::Body;
use futures::future;
use std::string::ToString;

mod path;
pub use path::{Path, AbsolutePath, RelativePath};
use super::sse;

// Exterior interface is opaque type `Subscription` which can only be
// constructed by deserializing it (i.e. from JSON)...

#[derive(Debug, Clone, Deserialize)]
pub struct Subscription(
    HashMap<AbsolutePath, HashMap<String, HashSet<Path>>>
);

#[derive(Debug, Clone, Serialize)]
pub struct AggregateSubscription<'a>(
    HashMap<&'a AbsolutePath, HashMap<&'a String, HashSet<&'a Path>>>
);

/// The maximum number of messages to buffer before blocking a send. This means
/// a client can send a burst of up to this number of UI events before it
/// experiences backpressure. It usually makes sense for this to be much higher
/// than the buffer size for page content, because page content is likely to be
/// larger and less "bursty."
const EVENT_BUFFER_SIZE: usize = 10_000;

#[derive(Debug)]
pub struct Subscribers {
    servers: Vec<Arc<Mutex<sse::BufferedServer>>>,
    routes: HashMap<AbsolutePath, HashMap<String, Vec<Sink>>>,
}

#[derive(Debug, Clone)]
struct Sink {
    return_paths: HashSet<Path>,
    server: Weak<Mutex<sse::BufferedServer>>,
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
    pub async fn add_subscriber<'a>(
        &'a mut self,
        subscription: Subscription,
    ) -> (Option<AggregateSubscription<'a>>, Body) {
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
        let mut server = sse::BufferedServer::new(EVENT_BUFFER_SIZE).await;
        let (sender, body) = Body::channel();
        server.add_client(sender).await;
        let server = Arc::new(Mutex::new(server));
        // Add a reference to the server, with the appropriate property filter,
        // to each place corresponding to its desired subscription.
        for (path, events) in subscription.0 {
            let existing_events =
                self.routes.entry(path.into()).or_insert_with(|| HashMap::new());
            for (event, return_paths) in events {
                let existing_sinks =
                    existing_events.entry(event).or_insert_with(|| Vec::new());
                existing_sinks.push(Sink{server: Arc::downgrade(&server), return_paths});
            }
        }
        // Add the server to the top-level list of server references
        self.servers.push(server);
        // Return the body, for sending to whoever subscribed
        (Some(self.total_subscription()), body)
    }

    /// Send an event to all subscribers to that event, giving each subscriber
    /// only those fields of the event which that subscriber cares about. If the
    /// list of subscribers has changed (that is, by client disconnection),
    /// returns the union of all now-current subscriptions.
    pub async fn send_event<'a>(&'a mut self,
                                event_type: &str,
                                event_path: &AbsolutePath,
                                event_data: &HashMap<Path, Value>
    ) -> Option<AggregateSubscription<'a>> {
        let mut subscription_changed = false;
        if let Some(sinks) =
            self.routes.get_mut(event_path).and_then(|m| m.get_mut(event_type)) {
                let mut sent = future::join_all(sinks.iter_mut().map(|sink| {
                    // Filter the fields of the event to those expected by this
                    // particular subscriber
                    let filtered: HashMap<&Path, &Value> =
                        sink.return_paths.iter().filter_map(|path| {
                            event_data.get(path).map(|value| (path, value))
                        }).collect();
                    // Serialize the fields to JSON
                    let data = serde_json::to_string(&filtered)
                        .expect("Serializing fields to a string shouldn't fail");
                    // Build a text/event-stream message to send to subscriber
                    let message =
                        EventBuilder::new(&data)
                        .id(&event_path.to_string())
                        .event_type(event_type)
                        .build();
                    // Make a future for sending the message to the subscriber
                    async move {
                        if let Some(server) = sink.server.upgrade() {
                            let remaining = server.lock().await
                                .send_to_clients(message).await.await;
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
        if let Some(events) = self.routes.get_mut(event_path) {
            if let Some(sinks) = events.get_mut(event_type) {
                if sinks.is_empty() {
                    events.remove(event_type);
                    subscription_changed = true;
                }
            }
            if events.is_empty() {
                self.routes.remove(event_path);
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
    pub async fn send_heartbeat<'a>(&'a mut self) -> Option<AggregateSubscription<'a>> {
        let mut sent = future::join_all(self.servers.iter_mut().map(|server| {
            async move {
                let remaining = server.lock().await.send_heartbeat().await.await;
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
        self.routes.retain(|_, events| {
            let previous_len = events.len();
            events.retain(|_, sinks| {
                let previous_len = sinks.len();
                sinks.retain(|Sink{server, ..}| {
                    if !server.upgrade().is_some() {
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
    pub fn total_subscription<'a>(&'a self) -> AggregateSubscription<'a> {
        let subscription =
            self.routes.iter().map(|(id, events)| {
                (id,
                 events.iter().map(|(event_type, sinks)| {
                     (event_type,
                      sinks.iter().fold(HashSet::new(), |all, Sink{return_paths, ..}| {
                          return_paths.iter().fold(all, |mut all, return_path| {
                              all.insert(return_path);
                              all
                          })
                      }))
                 }).collect())
            }).collect();
        AggregateSubscription(subscription)
    }
}
