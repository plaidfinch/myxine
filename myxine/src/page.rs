use hyper::body::{Body, Bytes};
use hyper_usse::EventBuilder;
use std::time::Duration;
use std::io::Write;
use tokio::sync::Mutex;
use futures::join;
use serde_json::Value;
use std::fmt::{Display, self};
use futures::{select, pin_mut, FutureExt};

mod subscription;
mod query;
mod content;
mod sse;

pub use subscription::{Subscription, Event};
use crate::unique::Unique;
use subscription::Subscribers;
use query::Queries;
use content::Content;
use super::server::RefreshMode;

/// A `Page` pairs some page `Content` (either dynamic or static) with a set of
/// `Subscribers` to the events on the page.
#[derive(Debug)]
pub struct Page {
    content: Mutex<Content>,
    subscribers: Mutex<Subscribers>,
    queries: Mutex<Queries<Result<Value, String>>>,
}

/// The possible errors that can occur while evaluating JavaScript in the
/// context of a page.
#[derive(Debug, Clone)]
pub enum EvalError {
    /// There was no browser viewing the page, so could not run JavaScript.
    NoBrowser,
    /// The page was static, so could not run JavaScript.
    StaticPage,
    /// An internal error caused the page handle to be dropped.
    NoResponsePossible,
    /// The page took too long to respond.
    Timeout { duration: Duration, was_custom: bool },
    /// The code threw some kind of exception in JavaScript.
    JsError(String),
}

impl Display for EvalError {
    fn fmt(&self, w: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use EvalError::*;
        match self {
            NoBrowser => write!(w, "No browser is currently viewing the page"),
            StaticPage => write!(w, "The page is static, which means it cannot run scripts"),
            NoResponsePossible => write!(w, "Internal error: page handle dropped"),
            Timeout{duration, was_custom} =>
                if *was_custom {
                    write!(w, "Exceeded custom timeout of {}ms", duration.as_millis())
                } else {
                    write!(w, "Exceeded default timeout of {}ms", duration.as_millis())
                },
            JsError(err) => write!(w, "Exception: {}", err)
        }
    }
}

/// The size of the dynamic page template in bytes
const TEMPLATE_SIZE: usize = include_str!("page/dynamic.html").len();

/// The default timeout for evaluating JavaScript expressions in the page,
/// measured in milliseconds
const DEFAULT_EVAL_TIMEOUT: Duration = Duration::from_millis(1000);

impl Page {
    /// Make a new empty (dynamic) page.
    pub fn new() -> Page {
        Page {
            content: Mutex::new(Content::new()),
            subscribers: Mutex::new(Subscribers::new()),
            queries: Mutex::new(Queries::new()),
        }
    }

    /// Render a whole page as HTML (for first page load).
    pub async fn render(&self) -> Body {
        match &*self.content.lock().await {
            Content::Dynamic{title, body, ..} => {
                let debug = cfg!(debug_assertions).to_string();

                // Pre-allocate a buffer exactly the right size
                let buffer_size =
                    TEMPLATE_SIZE + debug.len() + title.len() + body.len();
                let mut bytes = Vec::with_capacity(buffer_size);

                write!(&mut bytes,
                       include_str!("page/dynamic.html"),
                       debug = debug,
                       title = title,
                       body = body)
                    .expect("Internal error: write!() failed on a Vec<u8>");
                Body::from(bytes)
            },
            Content::Static{raw_contents, ..} => {
                raw_contents.clone().into()
            },
        }
    }

    /// Subscribe another page event listener to this page, given a subscription
    /// specification for what events to listen to.
    pub async fn event_stream(&self, subscription: Subscription) -> Body {
        let mut subscribers = self.subscribers.lock().await;
        subscribers.add_subscriber(subscription, true).await
    }

    /// Send an event to all subscribers. This should only be called with events
    /// that have come from the corresponding page itself, or confusion will
    /// result!
    pub async fn send_event(&self, event: &Event) {
        self.subscribers.lock().await.send_event(&event).await
    }

    /// Send an empty "heartbeat" message to all clients of a page, if it is
    /// dynamic. This has no effect if it is (currently) static, and returns
    /// `None` if so, otherwise returns the current number of clients getting
    /// live updates to the page.
    pub async fn send_heartbeat(&self) -> Option<usize> {
        let mut content = self.content.lock().await;
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
            Content::Dynamic{ref mut updates, ..} => {
                let (id, result) = self.queries.lock().await.request();
                let id_string = format!("{}", id);
                let event = EventBuilder::new(expression)
                    .event_type(if statement_mode { "run" } else { "evaluate" })
                    .id(&id_string);
                let client_count = updates.send(event.into());
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
                            let duration = timeout.unwrap_or(DEFAULT_EVAL_TIMEOUT);
                            tokio::time::delay_for(duration).await;
                            self.queries.lock().await.cancel(id);
                            Err(EvalError::Timeout{duration, was_custom: timeout.is_some()})
                        }.fuse();

                        // Wait for the result
                        let wait_result = async {
                            match result.await {
                                None => Err(EvalError::NoResponsePossible),
                                Some(result) => result.map_err(EvalError::JsError),
                            }
                        }.fuse();

                        // Race the timeout against the wait for the result
                        pin_mut!(request_timeout, wait_result);
                        select! {
                            result = request_timeout => result,
                            result = wait_result => result,
                        }
                    }
                }
            },
            Content::Static{..} => {
                return Err(EvalError::StaticPage);
            },
        }.await
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
        let (content, subscribers, queries) =
            join!(
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
        let mut content = self.content.lock().await;
        content.update_stream()
    }

    /// Set the contents of the page to be a static raw set of bytes with no
    /// self-refreshing functionality. All clients will be told to refresh their
    /// page to load the new static content (which will not be able to update
    /// itself until a client refreshes their page again).
    pub async fn set_static(&self,
                            content_type: Option<String>,
                            raw_contents: Bytes) {
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
    ) -> Body {
        let mut content = self.content.lock().await;
        content.set_title(new_title);
        content.set_body(new_body, refresh);
        Body::empty()
    }

    /// Clear the page entirely, removing all subscribers and resetting the page
    /// title and body to empty.
    pub async fn clear(&self) {
        let subscribers = &mut *self.subscribers.lock().await;
        *subscribers = Subscribers::new();
        let content = &mut *self.content.lock().await;
        content.set_title("");
        content.set_body("", RefreshMode::Diff);
    }
}
