use bytes::Bytes;
use serde::Serialize;
use std::mem;
use tokio::stream::{Stream, StreamExt};
use tokio::sync::broadcast;

use super::RefreshMode;
use crate::unique::Unique;

/// The `Content` of a page is either `Dynamic` or `Static`. If it's dynamic, it
/// has a title, body, and a set of SSE event listeners who are waiting for
/// updates to the page. If it's static, it just has a fixed content type and a
/// byte array of contents to be returned when fetched. `Page`s can be changed
/// from dynamic to static and vice-versa: when changing from dynamic to static,
/// the change is instantly reflected in the client web browser; in the other
/// direction, it requires a manual refresh (because a static page has no
/// injected javascript to make it update itself).
#[derive(Debug, Clone)]
pub enum Content {
    Dynamic {
        title: String,
        body: String,
        updates: broadcast::Sender<Command>,
        other_commands: broadcast::Sender<Command>,
    },
    Static {
        content_type: Option<String>,
        raw_contents: Bytes,
    },
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase", tag = "type")]
pub enum Command {
    Reload,
    Update {
        title: String,
        body: String,
        diff: bool,
    },
    Evaluate {
        script: String,
        statement_mode: bool,
        id: Unique,
    },
}

/// The maximum number of updates to buffer before dropping an update. This is
/// set to 1, because dropped updates are okay (the most recent update will
/// always get through once things quiesce).
const UPDATE_BUFFER_SIZE: usize = 1;

/// The maximum number of non-update commands to buffer before dropping one.
/// This is set to a medium sized number, because we don't want to drop a reload
/// command or an evaluate command. Unlike the update buffer, clients likely
/// won't fill this one, because it's used only for occasional full-reload
/// commands and for evaluating JavaScript, neither of which should be done at
/// an absurd rate.
const OTHER_COMMAND_BUFFER_SIZE: usize = 16;
// NOTE: This memory is allocated all at once, which means that the choice of
// buffer size impacts myxine's memory footprint.

impl Content {
    /// Make a new empty (dynamic) page
    pub fn new() -> Content {
        Content::Dynamic {
            title: String::new(),
            body: String::new(),
            updates: broadcast::channel(UPDATE_BUFFER_SIZE).0,
            other_commands: broadcast::channel(OTHER_COMMAND_BUFFER_SIZE).0,
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
                ref other_commands,
            } if title == "" && body == "" => {
                updates.receiver_count() == 0 && other_commands.receiver_count() == 0
            }
            _ => false,
        }
    }

    /// Add a client to the dynamic content of a page, if it is dynamic. If it
    /// is static, this has no effect and returns None. Otherwise, returns the
    /// Body stream to give to the new client.
    pub fn commands(&self) -> Option<impl Stream<Item = Command>> {
        let result = match self {
            Content::Dynamic {
                updates,
                other_commands,
                ..
            } => {
                let merged = updates.subscribe().merge(other_commands.subscribe());
                let stream_body = merged
                    .filter_map(|result| {
                        match result {
                            // We ignore lagged items in the stream! If we don't
                            // ignore these, we would terminate the Body on
                            // every lag, which is undesirable.
                            Err(broadcast::RecvError::Lagged(_)) => None,
                            // Otherwise, if the stream is over, we end this stream.
                            Err(broadcast::RecvError::Closed) => Some(Err(())),
                            // But if the item is ok, forward it.
                            Ok(item) => Some(Ok(item)),
                        }
                    })
                    .take_while(|i| i.is_ok())
                    .map(|i| i.unwrap());
                Some(stream_body)
            }
            Content::Static { .. } => None,
        };
        // Make sure the page is up to date
        self.refresh(RefreshMode::Diff);
        result
    }

    /// Tell all clients to refresh the contents of a page, if it is dynamic.
    /// This has no effect if it is (currently) static.
    pub fn refresh(&self, refresh: RefreshMode) {
        match self {
            Content::Dynamic {
                updates,
                other_commands,
                title,
                body,
            } => {
                let _ = match refresh {
                    RefreshMode::FullReload => other_commands.send(Command::Reload),
                    RefreshMode::SetBody | RefreshMode::Diff => updates.send(Command::Update {
                        title: title.clone(),
                        body: body.clone(),
                        diff: refresh == RefreshMode::Diff,
                    }),
                };
            }
            Content::Static { .. } => (),
        };
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
        content.refresh(RefreshMode::FullReload);
    }

    /// Tell all clients to change the body, if necessary. This converts the
    /// page into a dynamic page, overwriting any static content that previously
    /// existed, if any. Returns `true` if the page content was changed (either
    /// converted from static, or altered whilst dynamic).
    pub fn set(
        &mut self,
        new_title: impl Into<String>,
        new_body: impl Into<String>,
        refresh: RefreshMode,
    ) -> bool {
        let mut changed = false;
        loop {
            match self {
                Content::Dynamic {
                    ref mut title,
                    ref mut body,
                    ..
                } => {
                    let new_title = new_title.into();
                    let new_body = new_body.into();
                    if new_title != *title || new_body != *body {
                        changed = true;
                    }
                    *title = new_title;
                    *body = new_body;
                    break; // values have been set
                }
                Content::Static { .. } => {
                    *self = Content::new();
                    changed = true;
                    // and loop again to actually set
                }
            }
        }
        if changed {
            self.refresh(refresh);
        }
        changed
    }
}
