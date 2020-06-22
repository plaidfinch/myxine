use futures::join;
use futures::{pin_mut, select, FutureExt};
use hopscotch::{self, ArcK};
use hyper::body::{Body, Bytes};
use hyper_usse::EventBuilder;
use serde_json::Value;
use std::fmt::{self, Display};
use std::time::Duration;
use tokio::sync::{Mutex, RwLock};

mod content;
mod query;
mod subscription;

use super::server::RefreshMode;
use crate::unique::Unique;
use content::Content;
use query::Queries;
use subscription::Subscribers;
pub use subscription::{Event, Subscription};

/// A `Page` pairs some page `Content` (either dynamic or static) with a set of
/// `Subscribers` to the events on the page.
#[derive(Debug)]
pub struct Page {
    eval_timeout: RwLock<Duration>,
    content: Mutex<Content>,
    subscribers: Mutex<Subscribers>,
    queries: Mutex<Queries<Result<Value, String>>>,
    events: RwLock<(usize, hopscotch::Queue<String, Event, ArcK>)>, // TODO: preload events and use u16 tags
}

/// The possible errors that can occur while evaluating JavaScript in the
/// context of a page.
#[derive(Debug, Clone)]
pub enum EvalError {
    /// There was no browser viewing the page, so could not run JavaScript.
    NoBrowser,
    /// The page was static, so could not run JavaScript.
    StaticPage,
    /// The page took too long to respond.
    Timeout {
        duration: Duration,
        was_custom: bool,
    },
    /// The code threw some kind of exception in JavaScript.
    JsError(String),
}

impl Display for EvalError {
    fn fmt(&self, w: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use EvalError::*;
        match self {
            NoBrowser => write!(w, "No browser is currently viewing the page"),
            StaticPage => write!(w, "The page is static, which means it cannot run scripts"),
            Timeout {
                duration,
                was_custom,
            } => {
                if *was_custom {
                    write!(w, "Exceeded custom timeout of {}ms", duration.as_millis())
                } else {
                    write!(w, "Exceeded default timeout of {}ms", duration.as_millis())
                }
            }
            JsError(err) => write!(w, "Exception: {}", err),
        }
    }
}

impl Page {
    /// Make a new empty (dynamic) page.
    pub fn new(buffer_len: usize, eval_timeout: Duration) -> Page {
        Page {
            eval_timeout: RwLock::new(eval_timeout),
            content: Mutex::new(Content::new()),
            subscribers: Mutex::new(Subscribers::new()),
            queries: Mutex::new(Queries::new()),
            events: RwLock::new((buffer_len, hopscotch::Queue::new())),
        }
    }

    /// Render a whole page as HTML (for first page load).
    pub async fn render(&self) -> Body {
        match &*self.content.lock().await {
            Content::Dynamic { .. } => include_str!("page/dynamic.html").into(),
            Content::Static { raw_contents, .. } => raw_contents.clone().into(),
        }
    }

    /// Subscribe another page event listener to this page, given a subscription
    /// specification for what events to listen to.
    pub async fn event_stream(&self, subscription: Subscription) -> Body {
        let mut subscribers = self.subscribers.lock().await;
        subscribers.add_subscriber(subscription)
    }

    /// Get a specific event, at or after a particular moment in time, given a
    /// subscription specification that it must match. If the event is available
    /// in the buffer, this returns immediately, otherwise it may block until
    /// the event is available. If the request is for a moment which lies
    /// *before* the buffer, returns `Err(u64)`, to indicate the canonical
    /// moment for the event requested. The user-agent is responsible for
    /// retrying a request redirected to this moment.
    pub async fn event(&self, subscription: Subscription, moment: u64) -> Result<(u64, Body), u64> {
        let lagged = {
            // Scope for ensuring we drop events read-lock
            let events = &self.events.read().await.1;

            // Determine if the event was lagging the buffer
            let earliest_index = if let Some(earliest) = events.earliest() {
                earliest.index()
            } else {
                events.next_index()
            };
            let lagged = moment < earliest_index;

            match &subscription {
                Subscription::Universal => {
                    if lagged {
                        // Immediately report lag: since any event will do, the
                        // place to retry (after the user-agent re-issues the
                        // request) should be the earliest index represented.
                        return Err(earliest_index);
                    } else if let Some(found) = events.get(moment) {
                        // If no lag, and there is an event to send, send back
                        // the body we desire, and its found index.
                        let body = serde_json::to_string(found.value()).unwrap().into();
                        return Ok((moment, body));
                    } else {
                        // Block until we get a matching event (see below).
                    }
                }
                Subscription::Specific(tags) => {
                    if let Some(found) = events.after(moment, tags) {
                        if lagged {
                            // Immediately report lag: since it was found, the
                            // place to retry (after the user-agent re-issues
                            // the request) should be this very same index.
                            return Err(found.index());
                        } else {
                            // If no lag, send back the body we desire, and its
                            // found index.
                            let body = serde_json::to_string(found.value()).unwrap().into();
                            return Ok((found.index(), body));
                        }
                    } else {
                        // Block until we get a matching event (see below).
                    }
                }
            }

            // This computed value will be used below to determine whether to
            // report that lag occurred if there was both (1) lag, and (2) no
            // matching event yet.
            lagged
        };

        // If we couldn't find it in the buffered events, then wait for it:
        let result = {
            // Scope for ensuring we drop subscriber lock before awaiting the
            // result of the subscription
            let mut subscribers = self.subscribers.lock().await;
            subscribers.add_one_off(subscription, moment, lagged)
        };
        result.await
    }

    /// Get the next event for a particular subscription, bypassing the buffer
    /// of existing events and blocking until a new event arrives.
    pub async fn next_event(&self, subscription: Subscription) -> (u64, Body) {
        let result = {
            // Scope for ensuring we drop subscriber lock before awaiting the
            // result of the subscription
            let mut subscribers = self.subscribers.lock().await;
            subscribers.add_one_off(subscription, 0, false)
        };
        result.await.expect("Page::next_event can't lag")
    }

    /// Send an event to all subscribers. This should only be called with events
    /// that have come from the corresponding page itself, or confusion will
    /// result!
    pub async fn send_event(&self, event: Event) {
        let (ref max_buffer_len, ref mut events) = &mut *self.events.write().await;
        self.subscribers
            .lock()
            .await
            .send_event(events.next_index(), &event);
        let tag = event.event.clone();
        events.push(tag, event);
        // Trim the event buffer by a bounded amount if it's larger than the max
        // length. The bound prevents a sudden large decrease in max buffer
        // length from causing a long lag -- instead, the pruning is amortized
        // across many events, at the cost of not immediately freeing the
        // memory.
        for i in 0..10 {
            if events.len() > *max_buffer_len {
                events.pop();
            } else {
                // Shrink to fit only if we popped more than once: that's to
                // say, we only shrink the underlying buffer if we've actually
                // decreased its size in this method.
                if i > 1 && events.len() == *max_buffer_len {
                    events.shrink_to_fit();
                }
                break;
            }
        }
    }

    /// Send an empty "heartbeat" message to all clients of a page, if it is
    /// dynamic. This has no effect if it is (currently) static, and returns
    /// `None` if so, otherwise returns the current number of clients getting
    /// live updates to the page.
    pub async fn send_heartbeat(&self) -> Option<usize> {
        let content = self.content.lock().await;
        content.send_heartbeat()
    }

    /// Tell the page to evaluate a given piece of JavaScript, as either an
    /// expression or a statement, with an optional explicit timeout (defaults
    /// to the long-ish hardcoded timeout `DEFAULT_EVAL_TIMEOUT`).
    pub async fn evaluate(
        &self,
        expression: &str,
        statement_mode: bool,
        timeout: Option<Duration>,
    ) -> Result<Value, EvalError> {
        match *self.content.lock().await {
            Content::Dynamic {
                ref mut updates, ..
            } => {
                let (id, result) = self.queries.lock().await.request();
                let id_string = format!("{}", id);
                let event = EventBuilder::new(expression)
                    .event_type(if statement_mode { "run" } else { "evaluate" })
                    .id(&id_string);
                let client_count = updates.send(event.into()).unwrap_or(0);
                // All the below gets executed *after* the lock on content is
                // released, because it's a returned async block that is
                // .await-ed after the scope of the match closes.
                async move {
                    // If nobody's listening, give up now and report the issue
                    if client_count == 0 {
                        self.queries.lock().await.cancel(id);
                        Err(EvalError::NoBrowser)
                    } else {
                        // Timeout for evaluation request
                        let request_timeout = async move {
                            let duration = timeout.unwrap_or(*self.eval_timeout.read().await);
                            tokio::time::delay_for(duration).await;
                            self.queries.lock().await.cancel(id);
                            Err(EvalError::Timeout {
                                duration,
                                was_custom: timeout.is_some(),
                            })
                        }
                        .fuse();

                        // Wait for the result
                        let wait_result = async {
                            match result.await {
                                None => {
                                    panic!("Internal error: query handle dropped before response")
                                }
                                Some(result) => result.map_err(EvalError::JsError),
                            }
                        }
                        .fuse();

                        // Race the timeout against the wait for the result
                        pin_mut!(request_timeout, wait_result);
                        select! {
                            result = request_timeout => result,
                            result = wait_result => result,
                        }
                    }
                }
            }
            Content::Static { .. } => {
                return Err(EvalError::StaticPage);
            }
        }
        .await
    }

    /// Notify waiting clients of the result to some in-page JavaScript
    /// evaluation they have requested, either with an error or a valid
    /// response.
    pub async fn send_evaluate_result(&self, id: Unique, result: Result<Value, String>) {
        self.queries.lock().await.respond(id, result).unwrap_or(())
    }

    /// Test if this page is empty, where "empty" means that it is dynamic, with
    /// an empty title, empty body, and no subscribers waiting on its page
    /// events: that is, it's identical to `Page::new()`.
    pub async fn is_empty(&self) -> bool {
        let (content, subscribers, queries) = join!(
            async { self.content.lock().await },
            async { self.subscribers.lock().await },
            async { self.queries.lock().await },
        );
        content.is_empty() && subscribers.is_empty() && queries.is_empty()
    }

    /// Add a client to the dynamic content of a page, if it is dynamic. If it
    /// is static, this has no effect and returns None. Otherwise, returns the
    /// Body stream to give to the new client.
    pub async fn update_stream(&self) -> Option<Body> {
        let content = self.content.lock().await;
        content.update_stream()
    }

    /// Set the contents of the page to be a static raw set of bytes with no
    /// self-refreshing functionality. All clients will be told to refresh their
    /// page to load the new static content (which will not be able to update
    /// itself until a client refreshes their page again).
    pub async fn set_static(&self, content_type: Option<String>, raw_contents: Bytes) {
        let mut content = self.content.lock().await;
        content.set_static(content_type, raw_contents)
    }

    /// Get the content type of a page, or return `None` if none has been set
    /// (as in the case of a dynamic page, where the content type is not
    /// client-configurable).
    pub async fn content_type(&self) -> Option<String> {
        self.content.lock().await.content_type()
    }

    /// Tell all clients to change the title and body, if necessary. This
    /// converts the page into a dynamic page, overwriting any static content
    /// that previously existed, if any.
    pub async fn set_content(
        &self,
        new_title: impl Into<String>,
        new_body: impl Into<String>,
        refresh: RefreshMode,
    ) {
        self.content.lock().await.set(new_title, new_body, refresh);
    }

    /// Clear the page entirely, removing all subscribers and resetting the page
    /// title and body to empty.
    pub async fn clear(&self) {
        let subscribers = &mut *self.subscribers.lock().await;
        *subscribers = Subscribers::new();
        let content = &mut *self.content.lock().await;
        content.set("", "", RefreshMode::Diff);
    }

    /// Tell the page, if it is dynamic, to refresh its content in full from the
    /// server.
    #[allow(dead_code)]
    pub async fn refresh(&self) {
        self.content.lock().await.refresh(RefreshMode::FullReload)
    }
}
