use hyper::Body;
use hyper_usse::Event;
use std::mem;

#[derive(Debug)]
pub enum Page {
    Dynamic {
        title: String,
        body: String,
        sse: hyper_usse::Server,
    },
    Static {
        // TODO: Store and repeat the stated content-type for the thing
        raw_contents: Vec<u8>,
    }
}

impl Page {
    /// Render a whole page as HTML (for first page load)
    pub fn render(&self, event_source: &str) -> Vec<u8> {
        match self {
            Page::Dynamic{title, body, ..} => {
                render_dynamic(event_source, title, body).into()
            },
            Page::Static{raw_contents, ..} => {
                raw_contents.clone()
            },
        }
    }

    /// Make a new empty (dynamic) page
    pub fn new() -> Page {
        Page::Dynamic {
            title: String::new(),
            body: String::new(),
            sse: hyper_usse::Server::new()
        }
    }

    /// Add a client to the dynamic content of a page, if it is dynamic. If it
    /// is static, this has no effect and returns None. Otherwise, returns the
    /// Body stream to give to the new client.
    pub fn new_client(&mut self) -> Option<Body> {
        match self {
            Page::Dynamic{sse, ..} => {
                let (channel, body) = Body::channel();
                sse.add_client(channel);
                Some(body)
            },
            Page::Static{..} => None
        }
    }

    /// Tell all clients to refresh the contents of a page, if it is dynamic.
    /// This has no effect if it is (currently) static.
    pub async fn refresh(&mut self) {
        match self {
            Page::Dynamic{sse, ..} => {
                let event = Event::new(".").set_event("refresh").to_sse();
                sse.send_to_clients(event).await;
            },
            Page::Static{..} => { },
        }
    }

    /// Set the contents of the page to be a static raw set of bytes with no
    /// self-refreshing functionality. All clients will be told to refresh their
    /// page to load the new static content (which will not be able to update
    /// itself until a client refreshes their page again).
    pub async fn set_static(&mut self, raw_contents: impl Into<Vec<u8>>) {
        let mut page = Page::Static{raw_contents: raw_contents.into()};
        mem::swap(&mut page, self);
        page.refresh().await;
    }

    /// Tell all clients to change the title, if necessary. This converts the
    /// page into a dynamic page, overwriting any static content that previously
    /// existed, if any.
    pub async fn set_title(&mut self, new_title: impl Into<String>) {
        loop {
            match self {
                Page::Dynamic{ref mut title, ref mut sse, ..} => {
                    let new_title = new_title.into();
                    if new_title != *title {
                        *title = new_title.clone();
                        let event = if *title != "" {
                            Event::new(new_title).set_event("title")
                        } else {
                            Event::new(".").set_event("clear-title")
                        };
                        sse.send_to_clients(event.to_sse()).await;
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
                Page::Dynamic{ref mut body, ref mut sse, ..} => {
                    let new_body = new_body.into();
                    if new_body != *body {
                        *body = new_body.clone();
                        let event = if *body != "" {
                            Event::new(new_body).set_event("body")
                        } else {
                            Event::new(".").set_event("clear-body")
                        };
                        sse.send_to_clients(event.to_sse()).await;
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
}

/// Generate a "live page" with embedded SSE listeners, given an event source
/// URL, a page title, and HTML for the body.
// TODO: Add a full CSS reset here?
fn render_dynamic(event_source: &str, title: &str, body: &str) -> String {
    format!(include_str!("dynamic.html"),
            event_source = event_source,
            title = title,
            body = body)
}
