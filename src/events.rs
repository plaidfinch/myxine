use std::collections::HashMap;
use hyper_usse::EventBuilder;
use std::sync::{Arc, Weak};
use tokio::sync::Mutex;
use serde::{Serialize, Deserialize};
use serde_json::Value;
use hyper::Body;
use futures::future;

use crate::heartbeat;

// Exterior interface:

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct EventType(pub String);

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct Id(pub String);

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct FieldName(String);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Subscription(HashMap<Id, HashMap<EventType, Vec<FieldName>>>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Fields(HashMap<FieldName, Value>);


#[derive(Debug)]
struct Subscribers {
    inner: HashMap<Id, HashMap<EventType, Vec<Sink>>>,
}

#[derive(Debug, Clone)]
struct Sink {
    field_names: Vec<FieldName>,
    server: Weak<Mutex<hyper_usse::Server>>,
}

impl Subscribers {
    pub fn new() -> Subscribers {
        Subscribers { inner: HashMap::new() }
    }

    pub async fn add_subscription(&mut self, Subscription(spec): Subscription) -> Body {
        // Create a new single-client SSE server (new clients will never be
        // added after this, because each event subscription is potentially
        // unique).
        let mut server = hyper_usse::Server::new();
        let (sender, body) = Body::channel();
        server.add_client(sender);
        // Hand over the server to the heartbeat process, and receive a weak
        // reference which will be `None` if the connection is dropped in future
        let server = heartbeat::hold_subscriber(server).await;
        // Add a reference to the server, with the appropriate property filter,
        // to each place corresponding to its desired subscription.
        for (id, events) in spec {
            let existing_events =
                self.inner.entry(id).or_insert_with(|| HashMap::new());
            for (event, field_names) in events {
                let existing_sinks =
                    existing_events.entry(event).or_insert_with(|| Vec::new());
                existing_sinks.push(Sink{server: server.clone(), field_names});
            }
        }
        // Return the body, for sending to whoever subscribed
        body
    }

    pub async fn send_event(&mut self, event_type: &EventType, id: &Id, Fields(fields): &Fields) {
        if let Some(sinks) =
            self.inner.get_mut(id).and_then(|m| m.get_mut(event_type)) {
                let mut sent = future::join_all(sinks.iter_mut().map(|sink| {
                    let mut filtered_fields = HashMap::new();
                    for field_name in &sink.field_names {
                        if let Some(field) = fields.get(&field_name) {
                            filtered_fields.insert(field_name.clone(), field.clone());
                        }
                    }
                    let data = serde_json::to_string(&Fields(filtered_fields)).unwrap();
                    let message =
                        EventBuilder::new(&data)
                        .id(&id.0)
                        .event_type(&event_type.0)
                        .build();
                    async move {
                        if let Some(server) = sink.server.upgrade() {
                            1 == server.lock().await.send_to_clients(message).await
                        } else {
                            false
                        }
                    }
                })).await.into_iter();
                sinks.retain(|_| sent.next().unwrap());
            }
    }
}
