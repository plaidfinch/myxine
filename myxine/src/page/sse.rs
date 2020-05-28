use bytes::Bytes;
use tokio::sync::broadcast;
use tokio::stream::StreamExt;
use hyper::body::Body;

/// A broadcast channel allowing atomic writes of [`Bytes`] to any number of
/// streaming [`Body`] subscribers. This can be used to implement, for example,
/// an SSE mechanism.
#[derive(Debug, Clone)]
pub struct BroadcastBody {
    broadcast: broadcast::Sender<Bytes>,
}

impl BroadcastBody {
    /// Create a new `BroadcastBody` with a maximum lag of `size`. Consumers of
    /// `Body`s produced by this `BroadcastBody` will skip writes that lag by
    /// more than `size`.
    pub fn new(size: usize) -> Self {
        BroadcastBody {
            broadcast: broadcast::channel(size).0,
        }
    }

    /// Create a streaming [`Body`] that reflects all [`Bytes`] sent (except
    /// those which lag). This `Body` ends when the `BroadcastBody` itself is
    /// dropped.
    pub fn body(&self) -> Body {
        let rx = self.broadcast.subscribe();
        Body::wrap_stream(rx.filter_map(|result| {
            match result {
                // We ignore lagged items in the stream! If we don't ignore
                // these, we would terminate the Body on every lag, which is
                // undesirable.
                Err(broadcast::RecvError::Lagged(_)) => None,
                // Otherwise, we leave the result alone.
                result => Some(result),
            }
        }))
    }

    /// Send some [`Bytes`] to all currently subscribed [`Body`]s. This is sent
    /// atomically; that is, there will not be interleaving of `Bytes` between
    /// the inputs to individual calls to `send`.
    pub fn send(&self, text: Bytes) -> usize {
        self.broadcast.send(text).unwrap_or(0)
    }

    /// Count the number of [`Body`]s produced by this `BroadcastBody` which
    /// are live and receiving sent `Bytes`.
    pub fn connections(&self) -> usize {
        self.broadcast.receiver_count()
    }
}
