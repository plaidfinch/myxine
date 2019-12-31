use hyper::Body;
use hyper_usse::EventBuilder;
use std::io::Write;
use std::mem;
use futures::join;

use crate::events::{Subscribers, Subscription, Fields};

#[derive(Debug)]
pub enum Page {
    Dynamic {
        title: String,
        body: String,
        update_streams: hyper_usse::Server,
        event_subscribers: Subscribers,
    },
    Static {
        content_type: Option<String>,
        raw_contents: Vec<u8>,
    }
}

/// The size of the dynamic page template in bytes
const TEMPLATE_SIZE: usize = include_str!("dynamic.html").len();

impl Page {
    /// Make a new empty (dynamic) page
    pub fn new() -> Page {
        Page::Dynamic {
            title: String::new(),
            body: String::new(),
            update_streams: hyper_usse::Server::new(),
            event_subscribers: Subscribers::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Page::Dynamic{title, body, update_streams, event_subscribers}
            if title == "" && body == ""
                && update_streams.connections() == 0
                && event_subscribers.is_empty() => true,
            _ => false,
        }
    }

    /// Render a whole page as HTML (for first page load)
    pub fn render(&self, event_source: &str) -> Vec<u8> {
        match self {
            Page::Dynamic{title, body, ..} => {
                let mut bytes =
                    Vec::with_capacity(TEMPLATE_SIZE
                                       + event_source.len()
                                       + title.len()
                                       + body.len());
                // TODO: include generated javascript for subscriptions
                write!(&mut bytes,
                       include_str!("dynamic.html"),
                       event_source = event_source,
                       title = title,
                       body = body)
                    .expect("Internal error: write!() failed on a Vec<u8>");
                bytes
            },
            Page::Static{raw_contents, ..} => {
                raw_contents.clone()
            },
        }
    }

    /// Add a client to the dynamic content of a page, if it is dynamic. If it
    /// is static, this has no effect and returns None. Otherwise, returns the
    /// Body stream to give to the new client.
    pub fn update_stream(&mut self) -> Option<Body> {
        match self {
            Page::Dynamic{update_streams, ..} => {
                let (channel, body) = Body::channel();
                update_streams.add_client(channel);
                Some(body)
            },
            Page::Static{..} => None
        }
    }

    /// Send an empty "heartbeat" message to all clients of a page, if it is
    /// dynamic. This has no effect if it is (currently) static, and returns
    /// `None` if so, otherwise returns the current number of clients getting
    /// live updates to the page.
    pub async fn send_heartbeat(&mut self) -> Option<usize> {
        match self {
            Page::Dynamic{update_streams, event_subscribers, ..} => {
                // Send a heartbeat to pages waiting on <body> updates, as well
                // as to the subscribers waiting for page events
                let (updates_clients,
                     updated_subscriptions) =
                    join!(update_streams.send_heartbeat(),
                          event_subscribers.send_heartbeat());
                // If the heartbeat to subscribers revealed that some have
                // disconnected, update the client page with a new union set of
                // subscriptions
                if let Some(updated_subscriptions) = updated_subscriptions {
                    self.set_subscriptions(updated_subscriptions).await;
                }
                Some(updates_clients)
            },
            Page::Static{..} => None,
        }
    }

    /// Tell all clients to refresh the contents of a page, if it is dynamic.
    /// This has no effect if it is (currently) static.
    pub async fn refresh(&mut self) {
        match self {
            Page::Dynamic{update_streams, ..} => {
                let event = EventBuilder::new(".").event_type("refresh").build();
                update_streams.send_to_clients(event).await;
            },
            Page::Static{..} => { },
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
            Page::Static{content_type, raw_contents: raw_contents.into()};
        mem::swap(&mut page, self);
        page.refresh().await;
    }

    /// Get the content type of a static page, or return `None` if we should
    /// just use the normal `text/html; charset=utf8` that will be usually
    /// produced.
    pub fn content_type(&self) -> Option<String> {
        match self {
            Page::Dynamic{..} => None,
            Page::Static{content_type, ..} => content_type.clone(),
        }
    }

    /// Tell all clients to change the title, if necessary. This converts the
    /// page into a dynamic page, overwriting any static content that previously
    /// existed, if any.
    pub async fn set_title(&mut self, new_title: impl Into<String>) {
        loop {
            match self {
                Page::Dynamic{ref mut title, ref mut update_streams, ..} => {
                    let new_title = new_title.into();
                    if new_title != *title {
                        *title = new_title.clone();
                        let event = if *title != "" {
                            EventBuilder::new(&new_title).event_type("title")
                        } else {
                            EventBuilder::new(".").event_type("clear-title")
                        };
                        update_streams.send_to_clients(event.build()).await;
                    }
                    break; // title has been set
                },
                Page::Static{..} => {
                    *self = Page::new();
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
                Page::Dynamic{ref mut body, ref mut update_streams, ..} => {
                    let new_body = new_body.into();
                    if new_body != *body {
                        *body = new_body.clone();
                        let event = if *body != "" {
                            EventBuilder::new(&new_body).event_type("body")
                        } else {
                            EventBuilder::new(".").event_type("clear-body")
                        };
                        update_streams.send_to_clients(event.build()).await;
                    }
                    break; // body has been set
                },
                Page::Static{..} => {
                    *self = Page::new();
                    // and loop again to actually set the body
                }
            }
        }
    }

    /// Subscribe another page event listener to this page, given a subscription
    /// specification for what events to listen to.
    pub async fn event_stream(&mut self, subscription: Subscription) -> Body {
        match self {
            Page::Static{..} => Body::empty(),
            Page::Dynamic{event_subscribers, ..} => {
                let (total_subscription, body) =
                    event_subscribers.add_subscriber(subscription).await;
                if let Some(total_subscription) = total_subscription {
                    self.set_subscriptions(total_subscription).await;
                }
                body
            }
        }
    }

    /// Send an event to all subscribers. This should only be called with events
    /// that have come from the corresponding page itself, or confusion will
    /// result!
    pub(crate) async fn send_event(&mut self, event_type: &str, id: &str, fields: Fields) {
        match self {
            Page::Static{..} => { },
            Page::Dynamic{event_subscribers, ..} => {
                if let Some(total_subscription) =
                    event_subscribers.send_event(event_type, id, fields).await {
                        self.set_subscriptions(total_subscription).await;
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
    async fn set_subscriptions(&mut self, subscriptions: Subscription) {
        match self {
            Page::Static{..} => { },
            Page::Dynamic{update_streams, ..} => {
                let data = serde_json::to_string(&subscriptions)
                    .expect("Serializing subscriptions to JSON shouldn't fail");
                let event = EventBuilder::new(&data).event_type("subscribe");
                update_streams.send_to_clients(event.build()).await;
            }
        }
    }
}
