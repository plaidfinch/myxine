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
mod id;

pub use subscription::{Subscription, AggregateSubscription, Event};
pub use id::{Id, Global, Frame};
use subscription::Subscribers;
use query::Queries;
use content::Content;

/// A `Page` pairs some page `Content` (either dynamic or static) with a set of
/// `Subscribers` to the events on the page.
#[derive(Debug)]
pub struct Page {
    frame_id: Mutex<Id<Frame>>,
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
    pub async fn new() -> Page {
        Page {
            frame_id: Mutex::new(Id::new(Frame::new())),
            content: Mutex::new(Content::new().await),
            subscribers: Mutex::new(Subscribers::new()),
            queries: Mutex::new(Queries::new()),
        }
    }

    /// Increment the frame number for the page. This should be called
    /// internally whenever its contents have been updated.
    async fn next_frame(&self) -> Id<Frame> {
        let mut frame_id = self.frame_id.lock().await;
        *frame_id = Id::new(Frame::new());
        *frame_id
    }

    /// Get the current frame number for this page.
    async fn current_frame(&self) -> Id<Frame> {
        *self.frame_id.lock().await
    }

    /// Render a whole page as HTML (for first page load).
    pub async fn render(&self) -> Body {
        match &*self.content.lock().await {
            Content::Dynamic{title, body, ..} => {
                let debug = cfg!(debug_assertions).to_string();
                let frame_id = format!("\"{}\"", self.current_frame().await);
                let subscription = {
                    let subscribers = self.subscribers.lock().await;
                    let aggregate_subscription = subscribers.total_subscription();
                    serde_json::to_string(&aggregate_subscription).unwrap()
                };

                // Pre-allocate a buffer exactly the right size
                let buffer_size =
                    TEMPLATE_SIZE + subscription.len() + debug.len() + title.len() + body.len();
                let mut bytes = Vec::with_capacity(buffer_size);

                write!(&mut bytes,
                       include_str!("page/dynamic.html"),
                       debug = debug,
                       title = title,
                       frame_id = frame_id,
                       subscription = subscription,
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
    pub async fn event_stream(&self, subscription: Subscription) -> (Id<Global>, Body) {
        let mut subscribers = self.subscribers.lock().await;
        let (id, total_subscription, event_stream) =
            subscribers.add_persistent_subscriber(subscription).await;
        let frame_id = self.current_frame().await;
        let content = &mut *self.content.lock().await;
        content.set_subscriptions(frame_id, total_subscription).await;
        (id, event_stream)
    }

    /// Send an event to all subscribers. This should only be called with events
    /// that have come from the corresponding page itself, or confusion will
    /// result!
    pub async fn send_event(&self, frame_id: Id<Frame>, event: Event) {
        if let Some(total_subscription) =
            self.subscribers.lock().await
            .send_event(frame_id, event).await {
                let content = &mut *self.content.lock().await;
                content.set_subscriptions(frame_id, total_subscription).await;
            }
    }

    /// Send an empty "heartbeat" message to all clients of a page, if it is
    /// dynamic. This has no effect if it is (currently) static, and returns
    /// `None` if so, otherwise returns the current number of clients getting
    /// live updates to the page.
    pub async fn send_heartbeat(&self) -> Option<usize> {
        let mut content = self.content.lock().await;
        content.send_heartbeat().await
        // TODO: Send heartbeat to subscribers also? What heartbeat format to
        // use, given that it's not text/event-stream anymore?
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
                let get_client_count = updates.send_to_clients(event.build()).await;
                // All the below gets executed *after* the lock on content is
                // released, because it's a returned async block that is
                // .await-ed after the scope of the match closes.
                async move {
                    // If nobody's listening, give up now and report the issue
                    if get_client_count.await == 0 {
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
    pub async fn send_evaluate_result(&self, id: Id<Global>, result: Result<Value, String>) {
        self.queries.lock().await.respond(id, result).unwrap_or(())
    }

    /// Test if this page is empty, where "empty" means that it is dynamic, with
    /// an empty title, empty body, and no subscribers waiting on its page
    /// events: that is, it's identical to `Page::new()`.
    pub async fn is_empty(&self) -> bool {
        let (content_empty, subscribers_empty, queries_empty) =
            join!(
                async { self.content.lock().await.is_empty().await },
                async { self.subscribers.lock().await.is_empty() },
                async { self.queries.lock().await.is_empty() },
            );
        content_empty && subscribers_empty && queries_empty
    }

    /// Add a client to the dynamic content of a page, if it is dynamic. If it
    /// is static, this has no effect and returns None. Otherwise, returns the
    /// Body stream to give to the new client.
    pub async fn update_stream(&self) -> Option<Body> {
        let mut content = self.content.lock().await;
        content.update_stream().await
    }

    /// Set the contents of the page to be a static raw set of bytes with no
    /// self-refreshing functionality. All clients will be told to refresh their
    /// page to load the new static content (which will not be able to update
    /// itself until a client refreshes their page again).
    pub async fn set_static(&self,
                            content_type: Option<String>,
                            raw_contents: Bytes) {
        let mut content = self.content.lock().await;
        self.next_frame().await;
        content.set_static(content_type, raw_contents).await
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
        frame_subscription: Option<Subscription>,
    ) -> Body {
        // Get the next frame id for this page
        let frame_id = self.next_frame().await;
        // If the user wants to subscribe to events only on this frame, then set
        // up that transient subscription and return the event stream for the
        // single specified subscription. If there was no subscription
        // specified, returns the empty `Body`.
        if let Some(frame_subscription) = frame_subscription {
            // We only lock the subscribers in this scope, so that if the user
            // doesn't want events from this frame we don't pay the
            // synchronization penalty.
            let mut subscribers = self.subscribers.lock().await;
            let (new_aggregate, event_stream) =
                subscribers.add_transient_subscriber(frame_id, frame_subscription).await;
            // The content and subscribers locks need to overlap here because
            // the lifetime of the aggregate subscription is tied to the
            // lifetime of the subscribers.
            let mut content = self.content.lock().await;
            // We update the subscriptions before we update the body, to
            // make sure we don't miss any events.
            content.set_subscriptions(frame_id, new_aggregate).await;
            // Don't lock subscribers for any longer than is necessary
            drop(subscribers);
            // Update the title and body and note whether they've changed
            let title_changed = content.set_title(frame_id, new_title).await;
            let body_changed = content.set_body(frame_id, new_body).await;
            if title_changed || body_changed {
                event_stream
            } else {
                // If they haven't changed, then return an empty body. This
                // prevents the problem where an idempotent update gives back an
                // event stream that blocks and never yields events, until it
                // terminates when an event originating from a frame in its
                // future arrives, which kills it.
                Body::empty()
            }
        } else {
            // If there's no transient subscription required, then just set the
            // content and return the empty body
            let mut content = self.content.lock().await;
            content.set_title(frame_id, new_title).await;
            content.set_body(frame_id, new_body).await;
            Body::empty()
        }
    }

    /// Change a particular existing subscription stream's subscription,
    /// updating the aggregate subscription in the browser if necessary. Returns
    /// `Err(())` if the id given does not correspond to an extant stream.
    pub async fn change_subscription(&self, id: Id<Global>, subscription: Subscription) -> Result<(), ()> {
        match self.subscribers.lock().await.change_subscription(id, subscription) {
            Ok(Some(new_aggregate)) => {
                let frame_id = self.current_frame().await;
                self.content.lock().await.set_subscriptions(frame_id, new_aggregate).await;
                Ok(())
            },
            Ok(None) => Ok(()),
            Err(()) => Err(()),
        }
    }

    /// Clear the page entirely, removing all subscribers and resetting the page
    /// title and body to empty.
    pub async fn clear(&self) {
        let subscribers = &mut *self.subscribers.lock().await;
        *subscribers = Subscribers::new();
        let content = &mut *self.content.lock().await;
        let frame_id = self.next_frame().await;
        content.set_subscriptions(frame_id, AggregateSubscription::empty()).await;
        content.set_title(frame_id, "").await;
        content.set_body(frame_id, "").await;
    }
}
