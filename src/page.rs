use hyper::Body;
use hyper_usse::EventBuilder;
use std::io::Write;
use std::mem;
use tokio::sync::Mutex;
use std::collections::HashMap;
use futures::join;
use serde_json::Value;

pub mod events;
use events::{Subscribers, Subscription, AggregateSubscription, AbsolutePath, Path};

#[derive(Debug)]
pub struct Page {
    content: Mutex<Content>,
    subscribers: Mutex<Subscribers>,
}

impl Page {
    /// Make a new empty (dynamic) page
    pub fn new() -> Page {
        Page {
            content: Mutex::new(Content::new()),
            subscribers: Mutex::new(Subscribers::new()),
        }
    }

    /// Render a whole page as HTML (for first page load)
    pub async fn render(&self, base_url: &str, this_url: &str) -> Vec<u8> {
        match &*self.content.lock().await {
            Content::Dynamic{title, body, ..} => {
                let subscribers = self.subscribers.lock().await;
                let aggregate_subscription = subscribers.total_subscription();
                let subscription =
                    serde_json::to_string(&aggregate_subscription).unwrap();
                let mut bytes = Vec::with_capacity(TEMPLATE_SIZE);
                write!(&mut bytes,
                       include_str!("page/dynamic.html"),
                       base_url = base_url,
                       this_url = this_url,
                       subscription = subscription,
                       debug = cfg!(debug_assertions),
                       title = title,
                       body = body)
                    .expect("Internal error: write!() failed on a Vec<u8>");
                bytes
            },
            Content::Static{raw_contents, ..} => {
                raw_contents.clone()
            },
        }
    }

    /// Subscribe another page event listener to this page, given a subscription
    /// specification for what events to listen to.
    pub async fn event_stream(&self, subscription: Subscription) -> Body {
        match &mut *self.content.lock().await {
            Content::Static{..} => Body::empty(),
            Content::Dynamic{ref mut updates, ..} => {
                let mut subscribers = self.subscribers.lock().await;
                let (total_subscription, body) =
                    subscribers.add_subscriber(subscription).await;
                if let Some(total_subscription) = total_subscription {
                    set_subscriptions(updates, total_subscription).await;
                }
                body
            }
        }
    }

    /// Send an event to all subscribers. This should only be called with events
    /// that have come from the corresponding page itself, or confusion will
    /// result!
    pub async fn send_event(&self,
                            event_type: &str,
                            event_path: &AbsolutePath,
                            event_data: &HashMap<Path, Value>) {
        let content = &mut *self.content.lock().await;
        match content {
            Content::Static{..} => { },
            Content::Dynamic{ref mut updates, ..} => {
                if let Some(total_subscription) =
                    self.subscribers.lock().await
                    .send_event(event_type, event_path, event_data).await {
                        set_subscriptions(updates, total_subscription).await;
                    }
            }
        }
    }

    pub async fn send_heartbeat(&self) -> Option<usize> {
        let mut subscribers = self.subscribers.lock().await;
        let mut content = self.content.lock().await;
        let subscriber_heartbeat = async {
            subscribers.send_heartbeat().await
        };
        let content_heartbeat = async {
            content.send_heartbeat().await
        };
        let (new_subscription, update_client_count) =
            join!(subscriber_heartbeat, content_heartbeat);
        if let Some(total_subscription) = new_subscription {
            match *content {
                Content::Dynamic{ref mut updates, ..} => {
                    set_subscriptions(updates, total_subscription).await;
                },
                Content::Static{..} => { },
            }
        }
        update_client_count
    }

    pub async fn is_empty(&self) -> bool {
        let (content_empty, subscribers_empty) =
            join!(async { self.content.lock().await.is_empty() },
                  async { self.subscribers.lock().await.is_empty() });
        content_empty && subscribers_empty
    }

    pub async fn update_stream(&self) -> Option<Body> {
        self.content.lock().await.update_stream()
    }

    pub async fn set_static(&self,
                            content_type: Option<String>,
                            raw_contents: impl Into<Vec<u8>>) {
        self.content.lock().await.set_static(content_type, raw_contents).await
    }

    pub async fn content_type(&self) -> Option<String> {
        self.content.lock().await.content_type()
    }

    pub async fn set_title(&self, new_title: impl Into<String>) {
        self.content.lock().await.set_title(new_title).await
    }

    pub async fn set_body(&self, new_body: impl Into<String>) {
        self.content.lock().await.set_body(new_body).await
    }
}

#[derive(Debug)]
enum Content {
    Dynamic {
        title: String,
        body: String,
        updates: hyper_usse::Server,
    },
    Static {
        content_type: Option<String>,
        raw_contents: Vec<u8>,
    }
}

/// The size of the dynamic page template in bytes
const TEMPLATE_SIZE: usize = include_str!("page/dynamic.html").len();

impl Content {
    /// Make a new empty (dynamic) page
    pub fn new() -> Content {
        Content::Dynamic {
            title: String::new(),
            body: String::new(),
            updates: hyper_usse::Server::new(),
        }
    }

    /// Test if this page is empty, where "empty" means that it is dynamic, with
    /// an empty title, empty body, and no subscribers waiting on its page
    /// events: that is, it's identical to `Content::new()`.
    pub fn is_empty(&self) -> bool {
        match self {
            Content::Dynamic{title, body, updates}
            if title == "" && body == ""
                && updates.connections() == 0 => true,
            _ => false,
        }
    }

    /// Add a client to the dynamic content of a page, if it is dynamic. If it
    /// is static, this has no effect and returns None. Otherwise, returns the
    /// Body stream to give to the new client.
    pub fn update_stream(&mut self) -> Option<Body> {
        match self {
            Content::Dynamic{updates, ..} => {
                let (channel, body) = Body::channel();
                updates.add_client(channel);
                Some(body)
            },
            Content::Static{..} => None
        }
    }

    /// Send an empty "heartbeat" message to all clients of a page, if it is
    /// dynamic. This has no effect if it is (currently) static, and returns
    /// `None` if so, otherwise returns the current number of clients getting
    /// live updates to the page.
    pub async fn send_heartbeat(&mut self) -> Option<usize> {
        match self {
            Content::Dynamic{updates, ..} => {
                // Send a heartbeat to pages waiting on <body> updates
                Some(updates.send_heartbeat().await)
            },
            Content::Static{..} => None,
        }
    }

    /// Tell all clients to refresh the contents of a page, if it is dynamic.
    /// This has no effect if it is (currently) static.
    pub async fn refresh(&mut self) {
        match self {
            Content::Dynamic{updates, ..} => {
                let event = EventBuilder::new(".").event_type("refresh").build();
                updates.send_to_clients(event).await;
            },
            Content::Static{..} => { },
        }
    }

    /// Set the contents of the page to be a static raw set of bytes with no
    /// self-refreshing functionality. All clients will be told to refresh their
    /// page to load the new static content (which will not be able to update
    /// itself until a client refreshes their page again).
    pub async fn set_static(&mut self,
                            content_type: Option<String>,
                            raw_contents: impl Into<Vec<u8>>) {
        let mut page =
            Content::Static{content_type, raw_contents: raw_contents.into()};
        mem::swap(&mut page, self);
        page.refresh().await;
    }

    /// Get the content type of a page, or return `None` if none has been set
    /// (as in the case of a dynamic page, where the content type is not
    /// client-configurable).
    pub fn content_type(&self) -> Option<String> {
        match self {
            Content::Dynamic{..} => None,
            Content::Static{content_type, ..} => content_type.clone(),
        }
    }

    /// Tell all clients to change the title, if necessary. This converts the
    /// page into a dynamic page, overwriting any static content that previously
    /// existed, if any.
    pub async fn set_title(&mut self, new_title: impl Into<String>) {
        loop {
            match self {
                Content::Dynamic{ref mut title, ref mut updates, ..} => {
                    let new_title = new_title.into();
                    if new_title != *title {
                        *title = new_title.clone();
                        let event = if *title != "" {
                            EventBuilder::new(&new_title).event_type("title")
                        } else {
                            EventBuilder::new(".").event_type("clear-title")
                        };
                        updates.send_to_clients(event.build()).await;
                    }
                    break; // title has been set
                },
                Content::Static{..} => {
                    *self = Content::new();
                    // and loop again to actually set the title
                }
            }
        }
    }

    /// Tell all clients to change the body, if necessary. This converts the
    /// page into a dynamic page, overwriting any static content that previously
    /// existed, if any.
    pub async fn set_body(&mut self, new_body: impl Into<String>) {
        loop {
            match self {
                Content::Dynamic{ref mut body, ref mut updates, ..} => {
                    let new_body = new_body.into();
                    if new_body != *body {
                        *body = new_body.clone();
                        let event = if *body != "" {
                            EventBuilder::new(&new_body).event_type("body")
                        } else {
                            EventBuilder::new(".").event_type("clear-body")
                        };
                        updates.send_to_clients(event.build()).await;
                    }
                    break; // body has been set
                },
                Content::Static{..} => {
                    *self = Content::new();
                    // and loop again to actually set the body
                }
            }
        }
    }
}

/// Send a new total set of subscriptions to the page, so it can update its
/// event hooks. This function should *only* ever be called *directly* after
/// obtaining such a new set of subscriptions from adding a subscriber,
/// sending an event, or sending a heartbeat! It will cause unexpected loss
/// of messages if you arbitrarily set the subscriptions of a page outside
/// of these contexts.
async fn set_subscriptions<'a>(server: &'a mut hyper_usse::Server,
                               subscription: AggregateSubscription<'a>) {
    let data = serde_json::to_string(&subscription)
        .expect("Serializing subscriptions to JSON shouldn't fail");
    let event = EventBuilder::new(&data).event_type("subscribe");
    server.send_to_clients(event.build()).await;
}
