use futures::{future, select, pin_mut, FutureExt};
use std::collections::{HashSet, HashMap, hash_map::Entry};
use std::iter::Iterator;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{mpsc, Mutex};
use tokio::time;

use crate::page::Page;

/// The interval between heartbeats.
const HEARTBEAT_INTERVAL: Duration = Duration::from_secs(10);

pub struct Session {
    touch_path: mpsc::UnboundedSender<String>,
    active_paths: Arc<Mutex<HashSet<String>>>,
    pages: Arc<Mutex<HashMap<String, Arc<Page>>>>,
}

impl Session {
    /// Create a new session, starting a thread to maintain heartbeats to any
    /// Pages created in this session.
    pub async fn start() -> Session {
        let (touch_path, recv_path) = mpsc::unbounded_channel();
        let session = Session {
            touch_path,
            active_paths: Arc::new(Mutex::new(HashSet::new())),
            pages: Arc::new(Mutex::new(HashMap::new())),
        };
        let heartbeat = heartbeat_loop(
            recv_path,
            session.active_paths.clone(),
            session.pages.clone()
        );
        tokio::spawn(heartbeat);
        session
    }

    /// Retrieve or create a page at this path.
    pub async fn page(&self, path: &str) -> Arc<Page> {
        let page = match self.pages.lock().await.entry(path.to_string()) {
            Entry::Vacant(e) => {
                let page = Arc::new(Page::new());
                e.insert(page.clone());
                page
            },
            Entry::Occupied(e) => e.get().clone(),
        };

        // Make sure to send heartbeats to this page now
        self.touch_path.send(path.to_string()).unwrap_or(());

        page
    }
}

/// Send a heartbeat message to keep all page connections alive, simultaneously
/// pruning all pages from memory which have no content and no subscribers.
async fn heartbeat_loop(
    mut recv_path: mpsc::UnboundedReceiver<String>,
    active_paths: Arc<Mutex<HashSet<String>>>,
    pages: Arc<Mutex<HashMap<String, Arc<Page>>>>,
) {
    // Receive all new paths into the set of known active paths
    let recv_paths = async {
        while let Some(path) = recv_path.recv().await {
            active_paths.lock().await.insert(path);
        }
    }.fuse();

    // At the specified `HEARTBEAT_INTERVAL`, traverse all active paths, sending
    // heartbeats to all pages, and removing all pages which are identical to
    // the initial dynamic page (to free up memory).
    let heartbeat = async {
        loop {
            // Wait for next heartbeat interval...
            time::delay_for(HEARTBEAT_INTERVAL).await;

            // Lock the active set of paths and send a heartbeat to each one,
            // noting which paths are identical to the empty page
            let mut paths = active_paths.lock().await;
            let pruned = Arc::new(Mutex::new(Vec::new()));
            future::join_all(paths.iter().map(|path| {
                let pruned = pruned.clone();
                let pages = pages.clone();
                async move {
                    let mut pages = pages.lock().await;
                    if let Some((path, page)) = pages.remove_entry(path) {
                        page.send_heartbeat().await;
                        if !page.is_empty().await {
                            pages.insert(path, page);
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
    }.fuse();

    // Run them both concurrently, quit when session is dropped
    pin_mut!(recv_paths, heartbeat);
    select! {
        () = recv_paths => (),
        () = heartbeat => (),
    }
}
