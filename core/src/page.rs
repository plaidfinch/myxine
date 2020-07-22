use futures::{Stream, Future, FutureExt, select, join, pin_mut};
use bytes::Bytes;
use hopscotch::{self, ArcK};
use serde::Deserialize;
use serde_json::Value;
use tokio::sync::{Mutex, RwLock};
use std::sync::Arc;
use uuid::Uuid;

pub mod content;
pub mod query;
pub mod subscription;

pub use content::Content;
pub use query::Queries;
pub use subscription::Subscribers;
pub use subscription::{Event, Subscription};

/// A proxy for a page in the browser, encapsulating all the state and
/// connections necessary to set its content, listen for events, and evaluate
/// JavaScript within it. At any given time, a `Page` object may be connected to
/// zero, one, or more actual browser windows, each of which will exactly mirror
/// whatever is being done to the `Page` object.
///
/// To actually connect a `Page` to a real browser window, this library must be
/// used in concert with a web server to serve JavaScript that communicates with
/// the server to consume its streams of `Command`s and send it back browser
/// events. None of this functionality is contained in this crate, but for ease
/// of description, we will refer to the abstractions provided by a `Page` in
/// terms of its interactions with a real browser, even though this must be
/// enabled elsewhere (canonically, in the Myxine server).
///
/// A `Page` can correspond to dynamic or static content, and can be changed
/// between those two at will, though changing a page from static content to
/// dynamic content will necessitate a manual browser refresh to be noticeable,
/// since static content is served without any "magic" to make it automatically
/// change.
#[derive(Debug)]
pub struct Page {
    /// The current content of the page, including connections to any browser
    /// windows viewing that page.
    content: Mutex<Content>,
    /// The current subscribers to events in the page, including one-off pending
    /// requests for a particular event and persistent streams of many events.
    subscribers: Mutex<Subscribers>,
    /// The current pending JavaScript queries to the page.
    queries: Mutex<Queries<(String, bool), Result<Value, String>>>,
    /// The current buffer of events in the page, paired with the maximum buffer
    /// size limiter.
    events: RwLock<(BufferParams, hopscotch::Queue<String, Arc<Event>, ArcK>)>, // TODO: preload events and use u16 tags
}

/// The parameters defining the memory behavior of the event buffer.
#[derive(Debug, Clone)]
struct BufferParams {
    /// How many events does the buffer hold when full?
    size: usize,
    /// When the buffer is in the midst of being downsized, how many old events
    /// does it drop for each new event it receives?
    deallocation_rate: usize,
}

/// The possible responses from a page: either an event happened, or the result
/// of evaluating an expression was sent back.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase", tag = "type")]
pub enum Response {
    /// The browser sent back an event that occurred.
    Event(Event),
    /// The browser sent back the result of some JavaScript evaluation.
    EvalResult {
        /// The unique id of the JavaScript evaluation query.
        id: Uuid,
        /// The result of the query: either some successful JSON serialization
        /// of a JavaScript value, or the string representation of some
        /// JavaScript error.
        result: Result<Value, String>,
    },
}

/// The ways in which a page can be refreshed.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum RefreshMode {
    /// Reload the entire page via `window.location.reload()`.
    FullReload,
    /// Set the body of the page via `document.body = ...` (mainly to be used
    /// for debugging purposes).
    SetBody,
    /// The default: diff the update against the current page contents to update
    /// only the parts of the DOM which require it.
    Diff,
}

impl Page {
    /// Make a new empty (dynamic) page with a given internal event buffer size.
    /// This can be altered later by means of the `set_buffer_size` method.
    pub fn new(buffer_size: usize) -> Page {
        let params = BufferParams {
            size: buffer_size,
            deallocation_rate: 2,
        };
        Page {
            content: Mutex::new(Content::new()),
            subscribers: Mutex::new(Subscribers::new()),
            queries: Mutex::new(Queries::new()),
            events: RwLock::new((params, hopscotch::Queue::with_capacity(buffer_size))),
        }
    }

    /// Test if this page is empty, where "empty" means that it is dynamic, with
    /// an empty title, empty body, and no subscribers waiting on its page
    /// events.
    pub async fn is_empty(&self) -> bool {
        let (content, subscribers, queries) = join!(
            async { self.content.lock().await },
            async { self.subscribers.lock().await },
            async { self.queries.lock().await },
        );
        content.is_empty() && subscribers.is_empty() && queries.is_empty()
    }

    /// Get the stream of commands to this page, starting at the moment this
    /// function was called. These commands provide all the dynamic information
    /// necessary for a consumer of this stream to reconstruct the exact state
    /// of the page at any given moment. If the page is static, returns `None`.
    pub async fn commands(&self) -> Option<impl Stream<Item = content::Command>> {
        let mut content = self.content.lock().await;
        // Get the command stream (must come first here so we make sure to
        // capture these commands below).
        let commands = content.commands();
        // Re-send all pending evaluations to the page, in case it was static or
        // non-existent before.
        match &mut *content {
            Content::Dynamic { other_commands, .. } => {
                for (id, (script, statement_mode)) in self.queries.lock().await.pending() {
                    let command = content::Command::Evaluate {
                        script: script.clone(),
                        statement_mode: *statement_mode,
                        id: id.clone(),
                    };
                    other_commands.send(command).unwrap_or(0);
                }
            },
            Content::Static { .. } => {
                // Don't do anything if the content is static, because there's
                // no way to evaluate pending JavaScript on a static page
            }
        }
        commands
    }

    /// If the page is static, return a pair of its `Content-Type` (if it has
    /// one), and its static content. If the page is dynamic, returns `None`.
    pub async fn static_content(&self) -> Option<(Option<String>, Bytes)> {
        match &*self.content.lock().await {
            Content::Dynamic { .. } => None,
            Content::Static {
                content_type,
                raw_contents,
            } => Some((content_type.clone(), raw_contents.clone())),
        }
    }

    /// Get a stream of events which match the specified [`Subscription`],
    /// starting at the next event after the moment this function is called.
    pub async fn events(&self, subscription: Subscription) -> impl Stream<Item = Arc<Event>> {
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
    pub async fn event_after(&self, subscription: Subscription, moment: u64) -> Result<(u64, Arc<Event>), u64> {
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
                        return Ok((moment, found.value().clone()));
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
                            return Ok((found.index(), found.value().clone()));
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
    /// of pre-existing events and blocking until a new event arrives after the
    /// time this function was called.
    pub async fn next_event(&self, subscription: Subscription) -> (u64, Arc<Event>) {
        let result = {
            // Scope for ensuring we drop subscriber lock before awaiting the
            // result of the subscription
            let mut subscribers = self.subscribers.lock().await;
            subscribers.add_one_off(subscription, 0, false)
        };
        result.await.expect("Page::next_event can't lag")
    }

    /// Send an event to all subscribers to events on this page which are
    /// waiting on an event of this kind.
    pub async fn send_event(&self, event: Event) {
        let event = Arc::new(event);
        let (BufferParams{size: max_buffer_size, deallocation_rate},
             ref mut events) =
            &mut *self.events.write().await;
        self.subscribers
            .lock()
            .await
            .send_event(events.next_index(), event.clone());
        events.push(event.event.clone(), event);
        // Trim the event buffer by a bounded amount if it's larger than the max
        // length. The bound prevents a sudden large decrease in max buffer
        // length from causing a long lag -- instead, the pruning is amortized
        // across many events, at the cost of not immediately freeing the
        // memory.
        for i in 0..*deallocation_rate {
            if events.len() > *max_buffer_size {
                events.pop();
            } else {
                // Shrink to fit only if we popped more than once: that's to
                // say, we only shrink the underlying buffer if we've actually
                // decreased its size in this method.
                if i > 1 && events.len() == *max_buffer_size {
                    events.shrink_to_fit();
                }
                break;
            }
        }
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

    /// Set the contents of the page to be a static raw set of bytes with no
    /// self-refreshing functionality. All clients will be told to refresh their
    /// page to load the new static content (which will not be able to update
    /// itself until a client refreshes their page again).
    pub async fn set_static(&self, content_type: Option<String>, raw_contents: Bytes) {
        let mut content = self.content.lock().await;
        content.set_static(content_type, raw_contents)
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

    /// Tell the page to evaluate a given piece of JavaScript, as either an
    /// expression or a statement, with an optional explicit timeout (defaults
    /// to the timeout duration specified when this page was constructed).
    pub async fn evaluate(
        &self,
        expression: &str,
        statement_mode: bool,
        abort: impl Future<Output = ()>,
    ) -> Option<Result<Value, String>> {
        let script = expression.to_string();
        let (id, fut) = self.queries.lock().await.request((script.clone(), statement_mode));
        match *self.content.lock().await {
            Content::Dynamic {
                ref mut other_commands,
                ..
            } => {
                let command = content::Command::Evaluate {
                    script,
                    statement_mode,
                    id,
                };
                other_commands.send(command).unwrap_or(0);
            }
            Content::Static { .. } => {
                // Do nothing -- if the page is ever reloaded as dynamic, this
                // query will be re-sent to it if it hasn't yet been answered
            }
        }
        let abort = abort.fuse();
        let fut = fut.fuse();
        pin_mut!(fut, abort);
        select! {
            result = fut =>
                Some(result.expect("Internal error: query handle dropped before response")),
            () = abort => {
                self.queries.lock().await.cancel(id);
                None
            }
        }
    }

    /// Notify waiting clients of the result to some in-page JavaScript
    /// evaluation they have requested, either with an error or a valid
    /// response.
    pub async fn send_eval_result(&self, id: Uuid, result: Result<Value, String>) {
        let _ = self.queries.lock().await.respond(id, result);
    }

    /// Set the size of the page's internal event buffer. This will not actually
    /// de-allocate memory immediately, but rather will mean that future
    /// incoming events will incrementally trigger downsizing of the event
    /// buffer. When the event buffer actually reaches its new target size, one
    /// single re-allocation will occur to free the used memory.
    ///
    /// The `deallocation_rate` parameter determines how aggressively the buffer
    /// size is changed. A value of 2 means that for every incoming event, 2 old
    /// events are freed; a value of usize::max_value() means that the buffer is
    /// instantly resized at the next incoming event; anything in between cause
    /// incremental behavior, freeing N old events for each 1 new event. A
    /// deallocation rate of 0 or 1 is not valid, and will cause this function
    /// to panic.
    pub async fn set_buffer_size(&self, size: usize, deallocation_rate: usize) {
        if deallocation_rate >= 2 {
            self.events.write().await.0 =
                BufferParams { size, deallocation_rate };
        } else {
            panic!("Cannot set buffer size with a deallocation rate of < 2");
        }
    }
}
