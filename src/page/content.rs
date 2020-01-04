use hyper::Body;
use hyper_usse::EventBuilder;
use std::mem;

#[derive(Debug)]
pub enum Content {
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
