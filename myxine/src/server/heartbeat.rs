use lazy_static::lazy_static;
use std::collections::HashSet;
use std::iter::Iterator;
use std::time::Duration;
use std::sync::Arc;
use tokio::sync::{Mutex, mpsc};
use tokio::time;
use futures::{join, future};

use super::PAGES;

/// The interval between heartbeats, in seconds.
const HEARTBEAT_INTERVAL: u64 = 10;

lazy_static! {
    /// The sender and receiver, globally accessible, for incoming paths to keep
    /// alive via heartbeat.
    static ref TOUCHED_PATHS:
    (mpsc::UnboundedSender<String>,
     Arc<Mutex<mpsc::UnboundedReceiver<String>>>) = {
        let (send, recv) = mpsc::unbounded_channel();
        (send, Arc::new(Mutex::new(recv)))
    };

    /// The set of paths which are currently active and in need of heartbeats.
    static ref ACTIVE_PATHS: Mutex<HashSet<String>>
        = Mutex::new(HashSet::new());
}

/// Register a new server path as in need of page heartbeats. It will be
/// automatically de-registered when it is no longer alive. It's okay to call
/// this function whenever we see a path: we just want to make sure every path
/// receives heartbeats.
pub fn hold_path(path: String) {
    TOUCHED_PATHS.0.send(path).unwrap_or(());
}

/// Send a heartbeat message to keep all page connections alive, simultaneously
/// pruning all pages from memory which have no content and no subscribers.
pub async fn heartbeat_loop() {

    // Receive all new paths into the set of known active paths
    let recv_paths = async {
        let mut new_paths = TOUCHED_PATHS.1.lock().await;
        while let Some(path) = new_paths.recv().await {
            ACTIVE_PATHS.lock().await.insert(path);
        }
    };

    // At the specified `HEARTBEAT_INTERVAL`, traverse all active paths, sending
    // heartbeats to all pages, and removing all pages which are identical to
    // the initial dynamic page (to free up memory).
    let heartbeat = async {
        loop {
            // Wait for next heartbeat interval...
            time::delay_for(Duration::from_secs(HEARTBEAT_INTERVAL)).await;

            // Lock the active set of paths and send a heartbeat to each one,
            // noting which paths are identical to the empty page
            let mut paths = ACTIVE_PATHS.lock().await;
            let pruned = Arc::new(Mutex::new(Vec::new()));
            future::join_all(paths.iter().map(|path| {
                let pruned = pruned.clone();
                async move {
                    let mut pages = PAGES.lock().await;
                    if let Some((path, page)) = pages.remove_entry(path) {
                        page.send_heartbeat().await;
                        if !page.is_empty().await {
                            pages.insert(path, page);
                        } else {
                            pruned.lock().await.push(path);
                        }
                    }
                }
            })).await;

            // Remove all paths that are identical to the empty page
            for path in pruned.lock().await.iter() {
                paths.remove(path);
            }

            // Free memory for all the removed pages and paths
            paths.shrink_to_fit();
            PAGES.lock().await.shrink_to_fit();
        }
    };

    // Run them both concurrently
    join!(recv_paths, heartbeat);
}
