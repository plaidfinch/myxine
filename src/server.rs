use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, StatusCode, Uri};
use http::request::Parts;
use http::header::{HeaderMap, HeaderValue};
use lazy_static::lazy_static;
use std::sync::Arc;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::net::{SocketAddr, TcpListener};
use tokio::sync::Mutex;
use futures::stream::StreamExt;
use itertools::Itertools;

use crate::page::Page;
use crate::params::{RetrieveParams, PublishParams};
use crate::heartbeat;
use crate::windowing::WindowManager;

/// The mode of operation for the server
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Mode {
    /// The end of the server that browsers connect to, for rendering the view
    Interface,
    /// The end of the server that clients connect to, for controlling the view
    Control,
}

lazy_static! {
    /// The current contents of the server, indexed by path
    pub(crate) static ref PAGES:
    Mutex<HashMap<String, Arc<Mutex<Page>>>>
        = Mutex::new(HashMap::new());
}

/// Try to unwrap a `Result`, returning it if it is `Ok`. If it is an `Err`,
/// print the error to stderr, send `()` to the provided `Sender`, and return
/// early. (The lattermost part of this is why this has to be a macro and not a
/// function.)
macro_rules! unwrap_or_abort {
    ($e:expr) => {
        match $e {
            Err(err) => {
                eprintln!("{}", err);
                return;
            },
            Ok(result) => result,
        }
    };
}

/// Run the main server loop
pub async fn server(mode: Mode, socket_addr: SocketAddr, window_manager: WindowManager) {
    // Bind the server to this socket address
    let listener   = unwrap_or_abort!(TcpListener::bind(socket_addr));
    let local_addr = unwrap_or_abort!(listener.local_addr());
    let server     = unwrap_or_abort!(hyper::Server::from_tcp(listener));
    let base_uri   = unwrap_or_abort!(Uri::try_from(format!("http://{}", local_addr)));

    // Print the base URI to stdout: in a managed mode, a calling process could
    // read this to determine where to direct its future requests.
    match mode {
        Mode::Interface => {
            println!("View:    {}", base_uri.to_string().trim_end_matches('/'));
        },
        Mode::Control => {
            println!("Control: {}", base_uri.to_string().trim_end_matches('/'));
        },
    }

    unwrap_or_abort!(server.serve(make_service_fn(move |_| {
        let base_uri = base_uri.clone();
        let window_manager = window_manager.clone();
        async move {
            Ok::<_, hyper::Error>(service_fn(move |request: Request<Body>| {
                process_request(mode, base_uri.clone(), window_manager.clone(), request)
            }))
        }
    })).await);
}

async fn process_request(
    mode: Mode,
    base_uri: Uri,
    window_manager: WindowManager,
    request: Request<Body>,
) -> Result<Response<Body>, hyper::Error> {

    // Disassemble the request into the parts we care about
    let (parts, mut body) = request.into_parts();

    // More disassembly
    let Parts{method, uri, headers, ..} = parts;
    let query = uri.query().unwrap_or("");
    let path = uri.path().to_string();

    if cfg!(debug_assertions) {
        // Diagnostics about the requests we're receiving
        eprintln!("\n{} {}", method, uri);
        eprint_header(&headers, "Accept");
        eprint_header(&headers, "Content-Type");
    }

    // Get (or create) the page at this path. Note that this does *not* hold the
    // lock on PAGES, but rather extracts a clone of the `Arc<Mutex<Page>>` at
    // this path.
    let page = PAGES.lock().await
        .entry(path.clone())
        .or_insert_with(|| Arc::new(Mutex::new(Page::new())))
        .clone();

    // Make sure this path receives heartbeats
    heartbeat::touch_path(path.clone());

    // TODO: per-path backpressure to prevent overloading web browser

    // Just one big dispatch on the HTTP method...
    Ok(match (mode, method) {

        (Mode::Interface, Method::GET) => {
            let mut page = page.lock().await;
            // Parse the query parameters and proceed if they're okay
            if let Some(RetrieveParams{}) = RetrieveParams::parse(query) {

                // The client wants an event-stream if their Accept header says
                let wants_stream =
                    match headers.get("Accept").map(HeaderValue::to_str) {
                        None => false,
                        Some(Ok(accept)) => {
                            let accept = accept.to_owned().to_lowercase();
                            accept == "text/event-stream"
                        },
                        Some(Err(_)) =>
                            return Ok(bad_request("Invalid ASCII in Accept header.")),
                    };

                // If client wants event stream of changes to page:
                if wants_stream {
                    let body = page.new_client().unwrap_or_else(Body::empty);
                    Response::builder()
                        .header("Content-Type", "text/event-stream")
                        .header("Cache-Control", "no-cache")
                        .header("Access-Control-Allow-Origin", "*")
                        .body(body)
                        .unwrap()

                // If client wants entire full page:
                } else {
                    let event_stream_uri =
                        base_uri.to_string().trim_end_matches('/').to_owned()
                        + &path;
                    let body = page.render(&event_stream_uri).into();
                    let mut builder = Response::builder()
                        .header("Cache-Control", "no-cache");
                    if let Some(content_type) = page.content_type() {
                        // If there's a custom content-type, set it here
                        builder = builder.header("Content-Type", content_type);
                    }
                    builder.body(body).unwrap()
                }
            } else {
                return Ok(bad_request("Invalid query string in GET/HEAD."));
            }
        },

        (Mode::Control, Method::POST) => {
            // Slurp the body into memory
            // TODO: Accept text/event-stream to allow keep-alive connections
            let mut body_bytes: Vec<u8> = Vec::new();
            while let Some(chunk) = body.next().await {
                let chunk = chunk?;
                body_bytes.extend_from_slice(&chunk);
            };

            // TODO: routes: ?events gives UI event-stream (specified by the
            // JSON in the body), ?title=..., ?static=true/false,

            // The page should be dynamic if the Content-Type is missing or is
            // text/html or one of the defaults for `curl` (for convenience).
            // Here, returning `None` means make a dynamic page, and returning
            // `Some` means make a static page with that Content-Type.
            let static_content_type: Option<String> =
                match headers.get("Content-Type").map(HeaderValue::to_str) {
                    None => None,
                    Some(Ok(content_type)) => {
                        let content_type = content_type.to_lowercase();
                        let split_content_type: Vec<&str> =
                            content_type.split(';').map(|s| s.trim()).collect();
                        match split_content_type.as_slice()[0] {
                            "text/html" => None,
                            "application/x-www-form-urlencoded" => None,
                            "multipart/form-data" => None,
                            // The client specified a non-HTML content type,
                            // which we'll store statically without embedding in
                            // a fancy page
                            _ => Some(content_type.to_owned()),
                        }
                    },
                    Some(Err(_)) =>
                        return Ok(bad_request("Invalid ASCII in Content-Type header.")),
                };


            // Client wants to store a static file of a known Content-Type:
            if let Some(content_type) = static_content_type {
                let mut page = page.lock().await;
                page.set_static(content_type, body_bytes).await;

            // Client wants to publish some HTML to a dynamic page:
            } else if let Some(PublishParams{title}) = PublishParams::parse(query) {
                match String::from_utf8(body_bytes) {
                    Ok(body) => {
                        let mut page = page.lock().await;
                        let title = title.unwrap_or_else(|| "".to_string());
                        page.set_title(title.clone()).await;
                        page.set_body(body).await;
                        // If we are running in windowing mode, make sure we
                        // have a window for this path. Otherwise, this does
                        // nothing.
                        window_manager.ensure_window(format!("http://localhost:8000{}", path.trim_end_matches('/')), path, title);
                    },
                    Err(_) => return Ok(bad_request("Invalid UTF-8 in POST data (only UTF-8 is supported).")),
                };
            } else {
                return Ok(bad_request("Invalid query string in POST."));
            }

            // Give an empty response
            Response::new(Body::empty())
        },

        (Mode::Control, Method::DELETE) => {
            // This is equivalent to an empty-body POST with no query string
            if query != "" {
                return Ok(bad_request("Invalid query string in DELETE."));
            }
            let mut page = page.lock().await;
            page.set_title("").await;
            page.set_body("").await;
            Response::new(Body::empty())
        },

        _ => Response::builder()
            .status(StatusCode::METHOD_NOT_ALLOWED)
            .body(Body::empty())
            .unwrap(),
    })
}

// Some utilities for this module:

/// Assemble a BAD_REQUEST response with the given message
fn bad_request(message: impl Into<String>) -> Response<Body> {
    Response::builder()
        .status(StatusCode::BAD_REQUEST)
        .body(Body::from(message.into()))
        .unwrap()
}

/// Print a header value to stderr, for debugging purposes
fn eprint_header(headers: &HeaderMap<HeaderValue>, header: &str) {
    if headers.get(header).is_some() {
        eprint!("{}: ", header);
        eprintln!("{}", headers.get_all(header).iter()
                 .map(|host| host.to_str().unwrap_or("[invalid UTF-8]"))
                 .format(","));
    }
}
