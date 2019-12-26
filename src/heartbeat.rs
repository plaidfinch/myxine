use lazy_static::lazy_static;
use std::sync::Weak;
use std::collections::{HashMap, hash_map::Entry};
use std::iter::Iterator;
use std::time::Duration;
use tokio::sync::Mutex;
use tokio::time;
use futures::future;

use crate::page::Page;
use crate::server::PAGES;

const HEARTBEAT_INTERVAL: u64 = 10;

lazy_static! {
    /// A weak-ref copy of the server contents, for use with concurrent
    /// heartbeat-sending and garbage collection
    pub(crate) static ref HEARTBEAT_TABLE:
    Mutex<HashMap<String, Weak<Mutex<Page>>>>
        = Mutex::new(HashMap::new());
}

/// Send a heartbeat message to keep all page connections alive, simultaneously
/// pruning all pages from memory which have no content and no subscribers.
pub async fn heartbeat_loop() {
    // The result of sending a heartbeat to a page:
    enum HeartbeatResult {
        Alive, // The page is alive (either static, or dynamic and non-initial)
        Dropped(String), // We can forget about the page because it was dropped
        InitialState(String) // The page is in initial state and thus is a
                             // candidate for garbage collection (but we have to
                             // check in the main map to make sure it hasn't
                             // been updated during this GC cycle)
    }

    // At the specified `HEARTBEAT_INTERVAL`, traverse our weak copy of the page
    // list, removing all dangling weak pointers, sending heartbeats to all
    // pages, and removing all pages which are identical to the initial dynamic
    // page (to free up memory).
    tokio::spawn(async {
        loop {
            // Wait for next heartbeat interval...
            time::delay_for(Duration::from_secs(HEARTBEAT_INTERVAL)).await;

            // Traverse current known pages and send a heartbeat to all,
            // collecting the results of the traversal
            let mut pages_weak = HEARTBEAT_TABLE.lock().await;
            let heartbeat_results =
                pages_weak.iter_mut().map(|(path, page)| async move {
                    if let Some(page) = page.upgrade() {
                        let mut page = page.lock().await;
                        page.heartbeat().await;
                        if page.is_initial() {
                            HeartbeatResult::InitialState(path.clone())
                        } else {
                            HeartbeatResult::Alive
                        }
                    } else {
                        HeartbeatResult::Dropped(path.clone())
                    }
                });

            // Remove all dangling weak references we discovered from our own
            // map, and note all the paths which could correspond to ones to
            // prune in the strong map
            let mut potential_garbage: Vec<String> = Vec::new();
            for result in future::join_all(heartbeat_results).await {
                match result {
                    HeartbeatResult::Alive => { },
                    HeartbeatResult::Dropped(path) => {
                        pages_weak.remove(&path);
                    },
                    HeartbeatResult::InitialState(path) => {
                        potential_garbage.push(path);
                    }
                }
            }

            // Actually lock the main strong map of pages and garbage-collect
            // those pages which we've discovered to be in initial state... but
            // *only* if they're actually still in initial state
            for path in potential_garbage {
                let mut pages = PAGES.lock().await;
                match pages.entry(path) {
                    Entry::Vacant(_) => { },
                    Entry::Occupied(occupied) => {
                        let (path, page) = occupied.remove_entry();
                        if !page.lock().await.is_initial() {
                            // It got modified, so we should let it be by
                            // reinserting it into the table as-is
                            pages.insert(path, page);
                        } else {
                            // It was still initial, so forget about it here too
                            // This is not strictly-speaking necessary because
                            // we'll discover that the `Weak` is `None` in the
                            // next heartbeat loop, but this more eagerly
                            // conserves memory
                            pages_weak.remove(&path);
                        }
                    },
                }
            }
        }
    }).await.unwrap();
}
