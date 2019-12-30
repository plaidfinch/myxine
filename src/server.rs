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
use crate::params::{GetParams, PostParams};
use crate::heartbeat;

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
pub async fn server(socket_addr: SocketAddr) {
    // Bind the server to this socket address
    let listener   = unwrap_or_abort!(TcpListener::bind(socket_addr));
    let local_addr = unwrap_or_abort!(listener.local_addr());
    let server     = unwrap_or_abort!(hyper::Server::from_tcp(listener));
    let base_uri   = Arc::new(unwrap_or_abort!(Uri::try_from(format!("http://{}", local_addr))));

    // Print the base URI to stdout: in a managed mode, a calling process could
    // read this to determine where to direct its future requests.
    // println!("Running at: {}", base_uri.to_string().trim_end_matches('/'));

    unwrap_or_abort!(server.serve(make_service_fn(move |_| {
        let base_uri = base_uri.clone();
        async move {
            Ok::<_, hyper::Error>(service_fn(move |request: Request<Body>| {
                process_request(base_uri.clone(), request)
            }))
        }
    })).await);
}

async fn process_request(
    base_uri: Arc<Uri>,
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

    // Just one big dispatch on the HTTP method...
    Ok(match method {

        Method::GET | Method::HEAD => {
            let mut page = page.lock().await;
            let mut body = Body::empty();
            match GetParams::parse(query) {
                // If client wants event stream of changes to page:
                Some(GetParams::PageUpdates) => {
                    if method == Method::GET {
                        body = page.new_client().unwrap_or_else(Body::empty);
                    }
                    Response::builder()
                        .header("Content-Type", "text/event-stream")
                        .header("Cache-Control", "no-cache")
                        .header("Access-Control-Allow-Origin", "*")
                        .body(body)
                        .unwrap()
                },
                Some(GetParams::FullPage) => {
                    if method == Method::GET {
                        let event_stream_uri =
                            base_uri.to_string().trim_end_matches('/').to_owned()
                            + &path;
                        body = page.render(&event_stream_uri).into();
                    }
                    let mut builder = Response::builder()
                        .header("Cache-Control", "no-cache")
                        .header("Content-Disposition", "inline");
                    if let Some(content_type) = page.content_type() {
                        // If there's a custom content-type, set it here
                        builder = builder.header("Content-Type", content_type);
                    }
                    builder.body(body).unwrap()
                },
                None => {
                    return Ok(bad_request(if method == Method::GET {
                        "Invalid query string in GET."
                    } else {
                        ""
                    }));
                },
            }
        },

        Method::POST => {
            // Slurp the body into memory
            let mut body_bytes: Vec<u8> = Vec::new();
            while let Some(chunk) = body.next().await {
                let chunk = chunk?;
                body_bytes.extend_from_slice(&chunk);
            };

            let content_type: Option<&str> =
                match headers.get("Content-Type").map(HeaderValue::to_str) {
                    None => None,
                    Some(Ok(content_type)) =>
                        // One of these content types means it's just data, we
                        // know nothing about it, and we should just serve it up
                        // as a unicode string. The user should specify some
                        // particular content type if they desire one.
                        if content_type.starts_with("application/x-www-form-urlencoded")
                        || content_type.starts_with("multipart/form-data") {
                            None
                        } else {
                            Some(content_type)
                        },
                    Some(Err(_)) =>
                        return Ok(bad_request("Invalid ASCII in Content-Type header.")),
                };

            match PostParams::parse(query) {
                // Client wants to store a static file of a known Content-Type:
                Some(PostParams::StaticPage) => {
                    let mut page = page.lock().await;
                    page.set_static(content_type.map(String::from), body_bytes).await;
                    Response::new(Body::empty())
                },
                // Client wants to publish some HTML to a dynamic page:
                Some(PostParams::DynamicPage{title}) => {
                    match String::from_utf8(body_bytes) {
                        Ok(body) => {
                            let mut page = page.lock().await;
                            page.set_title(title).await;
                            page.set_body(body).await;
                            Response::new(Body::empty())
                        },
                        Err(_) =>
                            return Ok(bad_request("Invalid UTF-8 in POST data (only UTF-8 is supported).")),
                    }
                },
                // Client wants to subscribe to interface events on this page:
                Some(PostParams::SubscribeEvents) => {
                    todo!("Implement event subscription")
                },
                // Browser wants to notify client of an event
                Some(PostParams::PageEvent{event, id}) => {
                    todo!("Implement event notification from browser")
                }
                None => {
                    return Ok(bad_request("Invalid query string in POST."));
                }
            }
        },

        Method::DELETE => {
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
