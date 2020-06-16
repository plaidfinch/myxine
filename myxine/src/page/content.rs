use hyper::body::Bytes;
use hyper::Body;
use hyper_usse::EventBuilder;
use tokio::sync::broadcast;
use tokio::stream::StreamExt;
use std::mem;

use super::RefreshMode;

/// The `Content` of a page is either `Dynamic` or `Static`. If it's dynamic, it
/// has a title, body, and a set of SSE event listeners who are waiting for
/// updates to the page. If it's static, it just has a fixed content type and a
/// byte array of contents to be returned when fetched. `Page`s can be changed
/// from dynamic to static and vice-versa: when changing from dynamic to static,
/// the change is instantly reflected in the client web browser; in the other
/// direction, it requires a manual refresh (because a static page has no
/// injected javascript to make it update itself).
#[derive(Debug)]
pub enum Content {
    Dynamic {
        title: String,
        body: String,
        updates: broadcast::Sender<Bytes>,
    },
    Static {
        content_type: Option<String>,
        raw_contents: Bytes,
    },
}

/// The maximum number of messages to buffer before dropping an update. This is
/// set to 1, because dropped updates are okay (the most recent update will
/// always get through once things quiesce).
const UPDATE_BUFFER_SIZE: usize = 1;

impl Content {
    /// Make a new empty (dynamic) page
    pub fn new() -> Content {
        Content::Dynamic {
            title: String::new(),
            body: String::new(),
            updates: broadcast::channel(UPDATE_BUFFER_SIZE).0,
        }
    }

    /// Test if this page is empty, where "empty" means that it is dynamic, with
    /// an empty title, empty body, and no subscribers waiting on its page
    /// events: that is, it's identical to `Content::new()`.
    pub fn is_empty(&self) -> bool {
        match self {
            Content::Dynamic {
                title,
                body,
                ref updates,
            } if title == "" && body == "" => updates.receiver_count() == 0,
            _ => false,
        }
    }

    /// Add a client to the dynamic content of a page, if it is dynamic. If it
    /// is static, this has no effect and returns None. Otherwise, returns the
    /// Body stream to give to the new client.
    pub fn update_stream(&self) -> Option<Body> {
        match self {
            Content::Dynamic {
                updates,
                title,
                body,
            } => {
                // Composing the events to send: we send these in case the page
                // spontaneously reconnects to the server without having
                // refreshed cleanly, to make sure it gets an updated page right
                // away.
                let title_event = if *title != "" {
                    EventBuilder::new(&title).event_type("title")
                } else {
                    EventBuilder::new(".").event_type("clear-title")
                }
                .build();
                let body_event = if *body != "" {
                    EventBuilder::new(body).event_type("body")
                } else {
                    EventBuilder::new(".").event_type("clear-body")
                }
                .build();

                // NOTE: It's important that we construct the stream body here,
                // so that this update gets sent to the new stream as well
                let stream_body =
                    Body::wrap_stream(updates.subscribe().filter(|result| {
                        match result {
                            // We ignore lagged items in the stream! If we don't
                            // ignore these, we would terminate the Body on
                            // every lag, which is undesirable.
                            Err(broadcast::RecvError::Lagged(_)) => false,
                            // Otherwise, we leave the result alone.
                            _ => true,
                        }
                    }));

                // Then, send the events and return the stream body for
                // consumption by the client. We don't care about whether the
                // sends succeed, or how many clients they report -- if nobody's
                // connected, then that's fine.
                let _ = updates.send(title_event.into());
                let _ = updates.send(body_event.into());
                Some(stream_body)
            }
            Content::Static { .. } => None,
        }
    }

    /// Send an empty "heartbeat" message to all clients of a page, if it is
    /// dynamic. This has no effect if it is (currently) static, and returns
    /// `None` if so, otherwise returns the current number of clients getting
    /// live updates to the page.
    pub fn send_heartbeat(&self) -> Option<usize> {
        match self {
            Content::Dynamic { updates, .. } => {
                // Send a heartbeat to pages waiting on <body> updates
                Some(updates.send(":\n\n".into()).unwrap_or(0))
            }
            Content::Static { .. } => None,
        }
    }

    /// Tell all clients to refresh the contents of a page, if it is dynamic.
    /// This has no effect if it is (currently) static.
    pub fn refresh(&self) {
        match self {
            Content::Dynamic { updates, .. } => {
                let event = EventBuilder::new(".").event_type("refresh").build();
                // We don't care about whether the send succeeds, or how many
                // clients they report -- if nobody's connected, then that's
                // fine.
                let _ = updates.send(event.into());
            }
            Content::Static { .. } => {}
        }
    }

    /// Set the contents of the page to be a static raw set of bytes with no
    /// self-refreshing functionality. All clients will be told to refresh their
    /// page to load the new static content (which will not be able to update
    /// itself until a client refreshes their page again).
    pub fn set_static(&mut self, content_type: Option<String>, raw_contents: Bytes) {
        let mut content = Content::Static {
            content_type,
            raw_contents,
        };
        mem::swap(&mut content, self);
        content.refresh();
    }

    /// Get the content type of a page, or return `None` if none has been set
    /// (as in the case of a dynamic page, where the content type is not
    /// client-configurable).
    pub fn content_type(&self) -> Option<String> {
        match self {
            Content::Dynamic { .. } => None,
            Content::Static { content_type, .. } => content_type.clone(),
        }
    }

    /// Tell all clients to change the title, if necessary. This converts the
    /// page into a dynamic page, overwriting any static content that previously
    /// existed, if any. Returns `true` if the page content was changed (either
    /// converted from static, or altered whilst dynamic).
    pub fn set_title(&mut self, new_title: impl Into<String>) -> bool {
        let mut changed = false;
        loop {
            match self {
                Content::Dynamic {
                    ref mut title,
                    ref mut updates,
                    ..
                } => {
                    let new_title = new_title.into();
                    if new_title != *title {
                        *title = new_title;
                        changed = true;
                        let event = if title != "" {
                            EventBuilder::new(title).event_type("title")
                        } else {
                            EventBuilder::new(".").event_type("clear-title")
                        }
                        .build();
                        // We don't care about whether the send succeeds, or how
                        // many clients they report -- if nobody's connected,
                        // then that's fine.
                        let _ = updates.send(event.into());
                    }
                    break; // title has been set
                }
                Content::Static { .. } => {
                    *self = Content::new();
                    changed = true;
                    // and loop again to actually set the title
                }
            }
        }
        changed // report whether the page was changed
    }

    /// Tell all clients to change the body, if necessary. This converts the
    /// page into a dynamic page, overwriting any static content that previously
    /// existed, if any. Returns `true` if the page content was changed (either
    /// converted from static, or altered whilst dynamic).
    pub fn set_body(&mut self, new_body: impl Into<String>, refresh: RefreshMode) -> bool {
        let mut changed = false;
        loop {
            match self {
                Content::Dynamic {
                    ref mut body,
                    ref mut updates,
                    ..
                } => {
                    let new_body = new_body.into();
                    if new_body != *body {
                        *body = new_body;
                        changed = true;
                        // If refreshing whole page, do so; otherwise,
                        // diff-update
                        match refresh {
                            RefreshMode::FullReload => self.refresh(),
                            RefreshMode::SetBody | RefreshMode::Diff => {
                                let event = if body != "" {
                                    EventBuilder::new(body).event_type(
                                        if refresh == RefreshMode::Diff {
                                            "body"
                                        } else {
                                            "set-body"
                                        },
                                    )
                                } else {
                                    EventBuilder::new(".").event_type("clear-body")
                                }
                                .build();
                                // We don't care about whether the send
                                // succeeds, or how many clients they report --
                                // if nobody's connected, then that's fine.
                                let _ = updates.send(event.into());
                            }
                        }
                    }
                    break; // body has been set
                }
                Content::Static { .. } => {
                    *self = Content::new();
                    changed = true;
                    // and loop again to actually set the body
                }
            }
        }
        changed // report whether the page was changed
    }
}
