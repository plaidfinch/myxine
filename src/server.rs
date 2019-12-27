use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, StatusCode, Uri};
use http::request::Parts;
use http::header::{HeaderMap, HeaderValue};
use lazy_static::lazy_static;
use std::sync::Arc;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::net::SocketAddr;
use tokio::sync::{Mutex, oneshot};
use futures::stream::StreamExt;
use itertools::Itertools;

use crate::page::Page;
use crate::params::{RetrieveParams, PublishParams};
use crate::heartbeat;

lazy_static! {
    /// The current contents of the server, indexed by path
    pub(crate) static ref PAGES:
    Mutex<HashMap<String, Arc<Mutex<Page>>>>
        = Mutex::new(HashMap::new());
}

/// Run the main server loop
pub async fn server(socket_addr: SocketAddr, quit: oneshot::Sender<()>) {
    let base_uri =
        Arc::new(Uri::try_from(format!("http://{}", socket_addr)).unwrap());

    // Print the base URI to stdout: in a managed mode, a calling process could
    // read this to determine where to direct its future requests.
    println!("{}", base_uri);

    hyper::Server::bind(&socket_addr)
        .serve(make_service_fn(move |_| {
            let base_uri = base_uri.clone();
            async {
                Ok::<_, hyper::Error>(service_fn(move |request: Request<Body>| {
                    process_request(base_uri.clone(), request)
                }))
            }
        })).await.unwrap_or_else(|err| {
            eprintln!("{}", err);
            quit.send(()).unwrap_or(());
        })
}

fn bad_request(message: impl Into<String>) -> Response<Body> {
    Response::builder()
        .status(StatusCode::BAD_REQUEST)
        .body(Body::from(message.into()))
        .unwrap()
}

fn eprint_header(headers: &HeaderMap<HeaderValue>, header: &str) {
    if headers.get(header).is_some() {
        eprint!("{}: ", header);
        eprintln!("{}", headers.get_all(header).iter()
                 .map(|host| host.to_str().unwrap_or("[invalid UTF-8]"))
                 .format(","));
    }
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

    // TODO: per-path backpressure to prevent overloading web browser

    // Just one big dispatch on the HTTP method...
    Ok(match method {

        Method::GET | Method::HEAD => {
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
                    let body = if method == Method::HEAD {
                        Body::empty()
                    } else {
                        page.new_client().unwrap_or_else(Body::empty)
                    };
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
                    let body = if method == Method::HEAD {
                        Body::empty()
                    } else {
                        page.render(&event_stream_uri).into()
                    };
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

        Method::POST => {
            // Slurp the body into memory
            // TODO: Accept text/event-stream to allow keep-alive connections
            let mut body_bytes: Vec<u8> = Vec::new();
            while let Some(chunk) = body.next().await {
                let chunk = chunk?;
                body_bytes.extend_from_slice(&chunk);
            };

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
                        page.set_title(title.unwrap_or_else(|| "".to_string())).await;
                        page.set_body(body).await;
                    },
                    Err(_) => return Ok(bad_request("Invalid UTF-8 in POST data (only UTF-8 is supported).")),
                };
            } else {
                return Ok(bad_request("Invalid query string in POST."));
            }

            // Give an empty response
            Response::new(Body::empty())
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
