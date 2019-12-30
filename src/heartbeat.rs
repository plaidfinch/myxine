use lazy_static::lazy_static;
use std::collections::HashSet;
use std::iter::Iterator;
use std::time::Duration;
use std::sync::{Arc, Weak};
use tokio::sync::{Mutex, mpsc};
use tokio::time;
use futures::{join, future};

use crate::server::PAGES;

const HEARTBEAT_INTERVAL: u64 = 10;

lazy_static! {
    static ref TOUCHED_PATHS:
    (mpsc::UnboundedSender<String>,
     Arc<Mutex<mpsc::UnboundedReceiver<String>>>) = {
        let (send, recv) = mpsc::unbounded_channel();
        (send, Arc::new(Mutex::new(recv)))
    };

    static ref ACTIVE_PATHS: Mutex<HashSet<String>>
        = Mutex::new(HashSet::new());

    static ref LIVE_SUBSCRIBERS: Mutex<Vec<Arc<Mutex<hyper_usse::Server>>>>
        = Mutex::new(Vec::new());
}

pub fn hold_path(path: String) {
    TOUCHED_PATHS.0.send(path).unwrap_or(());
}

pub async fn hold_subscriber(server: hyper_usse::Server) -> Weak<Mutex<hyper_usse::Server>> {
    let server = Arc::new(Mutex::new(server));
    let weak_server = Arc::downgrade(&server);
    LIVE_SUBSCRIBERS.lock().await.push(server.clone());
    weak_server
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

            let process_pages = async {
                // Lock the active set of paths and send a heartbeat to each one,
                // noting which paths are identical to the empty page
                let mut paths = ACTIVE_PATHS.lock().await;
                let pruned = Arc::new(Mutex::new(Vec::new()));
                future::join_all(paths.iter().map(|path| {
                    let pruned = pruned.clone();
                    async move {
                        let mut pages = PAGES.lock().await;
                        if let Some((path, page)) = pages.remove_entry(path) {
                            let mut page_lock = page.lock().await;
                            page_lock.heartbeat().await;
                            if !page_lock.is_initial() {
                                drop(page_lock);
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
            };

            let process_subscribers = async {
                let mut subscribers = LIVE_SUBSCRIBERS.lock().await;
                let mut alive = future::join_all(subscribers.iter().map(|server| {
                    async move { 1 == server.lock().await.send_heartbeat().await }
                })).await.into_iter();
                subscribers.retain(|_| alive.next().unwrap());
            };

            join!(process_pages, process_subscribers);
        }
    };

    // Run them both concurrently
    join!(recv_paths, heartbeat);
}
