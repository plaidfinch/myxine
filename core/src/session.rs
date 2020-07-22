use futures::{future, pin_mut, select, FutureExt};
use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::iter::Iterator;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{mpsc, Mutex};
use tokio::time;

use crate::page::Page;

/// The configuration for a session, describing the behavior of the heartbeat /
/// page garbage collector, and the default parameters for each new page.
#[derive(Debug, Clone)]
pub struct Config {
    /// The delay between successive heartbeats to all pages.
    pub heartbeat_interval: Duration,
    /// The minimum time a page must remain untouched by any external method
    /// (including events coming in from the page itself) in order to be
    /// eligible for garbage collection.
    pub keep_alive_duration: Duration,
    /// The default size of the event buffer for each page. The larger this is,
    /// the more memory a page will consume, but clients will be able to lag
    /// by more events without dropping them.
    pub default_buffer_len: usize,
}

/// A collection of `Page`s, uniquely keyed by a `String` path, which are
/// periodically pruned by a garbage collector thread to remove inactive and
/// empty pages from the pool.
pub struct Session {
    touch_path: mpsc::UnboundedSender<String>,
    active_paths: Arc<Mutex<HashSet<String>>>,
    pages: Arc<Mutex<HashMap<String, (Instant, Arc<Page>)>>>,
    default_buffer_len: usize,
}

impl Session {
    /// Create a new session, starting a thread to maintain heartbeats to any
    /// Pages created in this session.
    pub async fn start(
        Config {
            heartbeat_interval,
            keep_alive_duration,
            default_buffer_len,
        }: Config,
    ) -> Session {
        let (touch_path, recv_path) = mpsc::unbounded_channel();
        let session = Session {
            touch_path,
            active_paths: Arc::new(Mutex::new(HashSet::new())),
            pages: Arc::new(Mutex::new(HashMap::new())),
            default_buffer_len,
        };
        let heartbeat = heartbeat_loop(
            heartbeat_interval,
            keep_alive_duration,
            recv_path,
            session.active_paths.clone(),
            session.pages.clone(),
        );
        tokio::spawn(heartbeat);
        session
    }

    /// Retrieve or create a page at this path.
    pub async fn page(&self, path: &str) -> Arc<Page> {
        let page = match self.pages.lock().await.entry(path.to_string()) {
            Entry::Vacant(e) => {
                let page = Arc::new(Page::new(self.default_buffer_len));
                e.insert((Instant::now(), page.clone()));
                page
            }
            Entry::Occupied(mut e) => {
                let (last_access, page) = e.get_mut();
                *last_access = Instant::now();
                page.clone()
            }
        };

        // Make sure to send heartbeats to this page now
        self.touch_path.send(path.to_string()).unwrap_or(());

        page
    }
}

/// Send a heartbeat message to keep all page connections alive, simultaneously
/// pruning all pages from memory which have no content and no subscribers.
async fn heartbeat_loop(
    interval: Duration,
    keep_alive: Duration,
    mut recv_path: mpsc::UnboundedReceiver<String>,
    active_paths: Arc<Mutex<HashSet<String>>>,
    pages: Arc<Mutex<HashMap<String, (Instant, Arc<Page>)>>>,
) {
    // Receive all new paths into the set of known active paths
    let recv_paths = async {
        while let Some(path) = recv_path.recv().await {
            active_paths.lock().await.insert(path);
        }
    }
    .fuse();

    // At the specified `HEARTBEAT_INTERVAL`, traverse all active paths, sending
    // heartbeats to all pages, and removing all pages which are identical to
    // the initial dynamic page (to free up memory).
    let heartbeat = async {
        loop {
            // Wait for next heartbeat interval...
            time::delay_for(interval).await;

            // Lock the active set of paths and send a heartbeat to each one,
            // noting which paths are identical to the empty page
            let mut paths = active_paths.lock().await;
            let pruned = Arc::new(Mutex::new(Vec::new()));
            future::join_all(paths.iter().map(|path| {
                let pruned = pruned.clone();
                let pages = pages.clone();
                async move {
                    let mut pages = pages.lock().await;
                    if let Some((path, (last_access, page))) = pages.remove_entry(path) {
                        if last_access.elapsed() < keep_alive || !page.is_empty().await {
                            pages.insert(path, (last_access, page));
                        } else {
                            pruned.lock().await.push(path);
                        }
                    }
                }
            }))
            .await;

            // Remove all paths that are identical to the empty page
            for path in pruned.lock().await.iter() {
                paths.remove(path);
            }

            // Free memory for all the removed pages and paths
            paths.shrink_to_fit();
            pages.lock().await.shrink_to_fit();
        }
    }
    .fuse();

    // Run them both concurrently, quit when session is dropped
    pin_mut!(recv_paths, heartbeat);
    select! {
        () = recv_paths => (),
        () = heartbeat => (),
    }
}
