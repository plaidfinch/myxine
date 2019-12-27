use std::collections::HashMap;
use std::sync::mpsc::{self, TryRecvError};

#[cfg(feature = "windowing")]
use web_view::{self, Content, WebView, WebViewBuilder};

const DEFAULT_WIDTH: i32 = 800;
const DEFAULT_HEIGHT: i32 = 600;

/// Create a channel to allow any threads to create a window on the main event
/// loop. Threads can clone the `WindowManager`, and the main event loop should
/// be called.
#[cfg(feature = "windowing")]
pub fn manager() -> (WindowManager, EventLoop) {
    let (tx, rx) = mpsc::channel();
    (WindowManager{tx}, EventLoop{rx})
}

#[cfg(not(feature = "windowing"))]
pub fn manager() -> (WindowManager, EventLoop) {
    (WindowManager{}, EventLoop{})
}

#[derive(Debug, Clone)]
#[cfg(feature = "windowing")]
pub struct WindowManager {
    tx: mpsc::Sender<(String, String, String)>,
}

#[derive(Debug, Clone)]
#[cfg(not(feature = "windowing"))]
pub struct WindowManager { }

#[derive(Debug)]
#[cfg(feature = "windowing")]
pub struct EventLoop {
    rx: mpsc::Receiver<(String, String, String)>,
}

#[derive(Debug)]
#[cfg(not(feature = "windowing"))]
pub struct EventLoop { }

impl WindowManager {
    #[cfg(not(feature = "windowing"))]
    pub fn ensure_window(&self, base_uri: String, path: String, title: String) {
        // Do nothing
    }

    #[cfg(feature = "windowing")]
    pub fn ensure_window(&self, base_uri: String, path: String, title: String) {
        self.tx.send((base_uri, path, title)).unwrap();
    }
}

/// This must be run from the main thread in order for windows to be visible!
impl EventLoop {

    #[cfg(not(feature = "windowing"))]
    pub fn run(&mut self) {
        // Do nothing
    }

    #[cfg(feature = "windowing")]
    pub fn run(&mut self) {
        let mut windows = HashMap::new();
        let mut channel_open = true;
        while channel_open || windows.len() > 0 {
            // Collect all the new windows created since last time we checked
            loop {
                if windows.is_empty() {
                    match self.rx.recv() {
                        Ok((uri, path, title)) =>
                            ensure_window(&mut windows, uri, path, title),
                        Err(_) => {
                            channel_open = false;
                            break;
                        },
                    }
                } else {
                    match self.rx.try_recv() {
                        Ok((uri, path, title)) =>
                            ensure_window(&mut windows, uri, path, title),
                        Err(TryRecvError::Empty) => break,
                        Err(TryRecvError::Disconnected) => {
                            channel_open = false;
                            break;
                        },
                    }
                }
            }
            // Step the event loop of each window, removing those which have
            // terminated from our map
            windows.retain(|_path, webview| {
                webview.step().is_some()
            });
        }
    }
}

fn ensure_window<'a>(windows: &mut HashMap<String, WebView<'a, ()>>,
                     base_uri: String, path: String, title: String) {
    // Only spawn a new window for this path if one doesn't already exist
    if let Some(webview) = windows.get_mut(&path) {
        webview.set_title(&title).unwrap_or(());
    } else {
        let webview = WebViewBuilder::new()
            .title("myxine")
            .content(Content::Url(&(base_uri + &path)))
            .size(DEFAULT_WIDTH, DEFAULT_HEIGHT)
            .resizable(true)
            .debug(true)
            .user_data(())
            .invoke_handler(|_webview, _arg| Ok(()))
            .build()
            .unwrap();
        windows.insert(path, webview);
    }
}
