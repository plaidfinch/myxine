use std::collections::{HashMap, HashSet};
use serde::{Serialize, Deserialize};
use serde_json::{Value, json};
use hyper::Body;
use futures::future;

use super::sse;
use crate::unique::Unique;

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

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(untagged)]
pub enum AggregateSubscription {
    Specific(HashSet<String>),
    Universal,
}

impl<'a> AggregateSubscription {
    /// Make an empty aggregate subscription (useful when specifying that a page
    /// should disconnect all open subscribers).
    pub fn empty() -> AggregateSubscription {
        AggregateSubscription::Specific(HashSet::new())
    }

    /// Add a single subscription to an aggregate.
    pub fn add(&mut self, subscription: Subscription) {
        if match self {
            AggregateSubscription::Universal => true,
            AggregateSubscription::Specific(ref mut existing) => {
                match subscription {
                    Subscription::Universal => true,
                    Subscription::Specific(new) => {
                        existing.extend(new.into_iter());
                        false
                    }
                }
            }
        } {
            *self = AggregateSubscription::Universal;
        }
    }

    /// Join together a bunch of individual `Subscription`s to form their
    /// aggregate.
    pub fn aggregate(
        subscriptions: impl IntoIterator<Item = &'a Subscription>
    ) -> AggregateSubscription {
        let mut aggregate = AggregateSubscription::empty();
        for subscription in subscriptions {
            aggregate.add(subscription.clone());
        }
        aggregate
    }

    /// Determine whether the given subscription is entirely subsumed by this
    /// one (that is, all its events are already present in the aggregate)
    pub fn subsumes(&self, subscription: &Subscription) -> bool {
        match self {
            AggregateSubscription::Universal => true,
            AggregateSubscription::Specific(aggregate_events) => {
                match subscription {
                    Subscription::Universal => false,
                    Subscription::Specific(these_events) => {
                        for event in these_events {
                            if !aggregate_events.contains(event) {
                                return false;
                            }
                        }
                        true
                    },
                }
            },
        }
    }
}

/// The maximum number of messages to buffer before dropping a message (i.e. the
/// maximum a client can lag in listening for subscribed events).
const EVENT_BUFFER_SIZE: usize = 10_000;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct SinkId(Unique);

#[derive(Debug)]
pub struct Subscribers {
    sinks: HashMap<SinkId, Sink>,
}

#[derive(Debug)]
struct Sink {
    subscription: Subscription,
    server: sse::BroadcastBody,
}

impl Subscribers {
    /// Make a new empty set of subscribers.
    pub fn new() -> Subscribers {
        Subscribers {
            sinks: HashMap::new(),
        }
    }

    /// Returns `true` if there are no subscribers to any events in this set of
    /// subscribers, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.sinks.is_empty()
    }

    /// Internal helper function to add a subscriber with a given SinkId (which
    /// should be generated elsewhere and returned from the public interface).
    async fn add_subscriber_with_id(
        &mut self, id: SinkId, subscription: Subscription
    ) -> (Option<AggregateSubscription>, Body) {
        // Create a new single-client SSE server (new clients will never be added
        // after this, because each event subscription is potentially unique).
        let server = sse::BroadcastBody::new(EVENT_BUFFER_SIZE);
        let body = server.body();

        // Determine if the aggregate subscription has changed as a result of
        // this new subscription being added
        let pre_existing_subscriptions =
            self.sinks.values().map(|Sink{subscription, ..}| subscription);
        let mut aggregate =
            AggregateSubscription::aggregate(pre_existing_subscriptions);
        let new_aggregate = if aggregate.subsumes(&subscription) {
            None
        } else {
            aggregate.add(subscription.clone());
            Some(aggregate)
        };

        // Insert the server into the sinks map
        self.sinks.insert(id, Sink{server, subscription});

        // Return the body, and new aggregate, for sending to whoever subscribed
        (new_aggregate, body)
    }

    /// Add a new *persistent* subscription to this set of subscribers,
    /// returning a streaming body to be sent to the subscribing client. The
    /// returned `Body` will not terminate until the consumer disconnects or the
    /// server exits.
    pub async fn add_persistent_subscriber(
        &mut self,
        subscription: Subscription,
    ) -> (Unique, Option<AggregateSubscription>, Body) {
        let id = Unique::new();
        let (aggregate, body) =
            self.add_subscriber_with_id(SinkId(id), subscription).await;
        (id, aggregate, body)
    }

    /// Send an event to all subscribers to that event, giving each subscriber
    /// only those fields of the event which that subscriber cares about. If the
    /// list of subscribers has changed (that is, by client disconnection),
    /// returns the union of all now-current subscriptions.
    pub async fn send_event<'a>(
        &'a mut self, event: Event
    ) -> Option<AggregateSubscription> {
        send_to_all(&mut self.sinks, &event).await
    }

    /// Change the subscription for a particular persistent subscriber,
    /// identified by UUID. If there is no such subscriber, returns `Err(())`.
    /// Otherwise, returns the new aggregate subscription, if it has changed, or
    /// `None` if it has not.
    pub fn change_subscription<'a>(
        &'a mut self, id: Unique, subscription: Subscription,
    ) -> Result<Option<AggregateSubscription>, ()> {
        let key = SinkId(id);
        match self.sinks.remove(&key) {
            None => Err(()),
            Some(Sink{server, subscription: old_subscription}) => {
                // This check for whether we should report a changed aggregate
                // subscription is overly conservative: if a subscription is
                // genuinely changed, but that change has no effect on the
                // aggregate, this will still report a changed aggregate. This
                // makes the common case of an un-changed single subscription
                // fast, and only decreases speed in the case where multiple
                // subscribers have differing, overlapping, changing
                // subscriptions.
                if old_subscription != subscription {
                    self.sinks.insert(key, Sink{server, subscription});
                    let new_aggregate = self.total_subscription();
                    Ok(Some(new_aggregate))
                } else {
                    self.sinks.insert(key, Sink{server, subscription: old_subscription});
                    Ok(None)
                }
            }
        }
    }

    /// Calculate the union of all subscriptions currently active.
    pub fn total_subscription(&self) -> AggregateSubscription {
        let subscriptions =
            self.sinks.values().map(|Sink{subscription, ..}| subscription);
        AggregateSubscription::aggregate(subscriptions)
    }

}

/// Send an event to all the sinks in the given `HashMap`, pruning the `HashMap`
/// to remove all those sinks which have become disconnected.
async fn send_to_all<'a>(
    sinks: &'a mut HashMap<SinkId, Sink>,
    event: &Event,
) -> Option<AggregateSubscription> {
    // The collection of futures for sending the event
    // Each future has Output = Option<Uuid>, where Some indicates that the sink
    // by that id **should** be pruned.
    let send_futures = sinks.iter_mut().map(move |(sink_id, sink)| {
        // Make a future for sending the message to the subscriber
        async move {
            let Event{event: event_type, properties, targets} = event;
            let remaining =
                // Message was a normal message
                if sink.subscription.matches_event(&event_type) {
                    // Serialize the fields to JSON
                    let message = serde_json::to_string(&json!({
                        "event": event_type,
                        "properties": properties,
                        "targets": targets,
                    })).expect("Serializing to a string shouldn't fail");
                    Some(sink.server.send((message + "\n").into()))
                } else {
                    None
                };
            // Return the sinks id to be pruned if it lost the client
            remaining.and_then(|r| {
                assert!(r <= 1, "Subscriber SSE exceeds 1 client");
                if r == 0 { Some(*sink_id) } else { None }
            })
        }
    });

    // Send all the events and remove all sinks that failed to send (client
    // disconnected), which are listed as `Some(sink_id)` in the results of the
    // sending.
    let mut subscription_changed = false;
    for closed_id in future::join_all(send_futures).await {
        if let Some(sink_id) = closed_id {
            sinks.remove(&sink_id);
            subscription_changed = true;
        }
    }
    // Shrink down the sinks so we don't bloat memory
    sinks.shrink_to_fit();

    // Calculate the new aggregate subscription
    if subscription_changed {
        Some(AggregateSubscription::aggregate(
            sinks.values().map(|Sink{subscription, ..}| subscription)
        ))
    } else {
        None
    }
}
