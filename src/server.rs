use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, StatusCode, Uri};
use http::request::Parts;
use http::header::HeaderValue;
use lazy_static::lazy_static;
use std::sync::Arc;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::mem;
use std::net::SocketAddr;
use tokio::sync::{Mutex, oneshot};
use futures::stream::StreamExt;

use crate::page::Page;
use crate::heartbeat::HEARTBEAT_TABLE;
use crate::params::{RetrieveParams, PublishParams};

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

    hyper::Server::bind(&socket_addr)
        .serve(make_service_fn(move |_| {
            let base_uri = base_uri.clone();
            async {
                Ok::<_, hyper::Error>(service_fn(move |request: Request<Body>| {
                    let base_uri = base_uri.clone();
                    async move {
                        // TODO: Reset no-activity timeout here
                        process_request(base_uri.clone(), request).await
                    }
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


async fn process_request(
    base_uri: Arc<Uri>,
    request: Request<Body>,
) -> Result<Response<Body>, hyper::Error> {

    // Disassemble the request into the parts we care about
    let (parts, mut body) = request.into_parts();

    println!("{:?}", parts);

    // More disassembly
    let Parts{method, uri, headers, ..} = parts;
    let query = uri.query().unwrap_or("");
    let path = uri.path().to_string();

    // Get (or create) the page at this path
    let mut pages = PAGES.lock().await;
    let page = pages.entry(path.clone())
        .or_insert(Arc::new(Mutex::new(Page::new())));

    // Insert a weak reference to this page into the heartbeat table
    HEARTBEAT_TABLE.lock().await
        .insert(path.clone(), Arc::downgrade(&page.clone()));

    // Await the lock on the page itself so we can modify it
    let mut page = page.lock().await;

    let result = match method {
        Method::GET | Method::HEAD => {
            if let Some(RetrieveParams{}) = RetrieveParams::parse(query) {
                // The client wants an event-stream if they say so!
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
                if wants_stream {
                    // Client wants event stream of changes to page
                    let body = if method == Method::HEAD {
                        Body::empty()
                    } else {
                        page.new_client().unwrap_or(Body::empty())
                    };
                    Response::builder()
                        .header("Content-Type", "text/event-stream")
                        .header("Cache-Control", "no-cache")
                        .header("Access-Control-Allow-Origin", "*")
                        .body(body)
                        .unwrap()
                } else {
                    // Client wants entire full page
                    let event_stream_uri =
                        base_uri.to_string().trim_end_matches("/").to_owned()
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
            let mut body_bytes: Vec<u8> = Vec::new();
            while let Some(chunk) = body.next().await {
                let chunk = chunk?;
                body_bytes.extend_from_slice(&chunk);
            };
            // The page should be dynamic if the Content-Type is missing or is
            // "text/html" or is "text/html; charset=utf-8".
            let static_content_type: Option<String> =
                match headers.get("Content-Type").map(HeaderValue::to_str) {
                    None => None,
                    Some(Ok(content_type)) => {
                        let content_type = content_type.to_lowercase();
                        let split_content_type: Vec<&str> =
                            content_type.split(";").map(|s| s.trim()).collect();
                        match split_content_type.as_slice() {
                            [] => None,
                            ["text/html", ..] => None,
                            ["application/x-www-form-urlencoded", ..] => None,
                            ["multipart/form-data", ..] => None,
                            // The client specified a non-HTML content type,
                            // which we'll store statically without embedding in
                            // a fancy page
                            _ => Some(content_type.to_owned()),
                        }
                    },
                    Some(Err(_)) =>
                        return Ok(bad_request("Invalid ASCII in Content-Type header.")),
                };

            // Dispatch on the detected content type to decide how to store it
            if let Some(content_type) = static_content_type {
                page.set_static(content_type, body_bytes).await;
            } else {
                if let Some(PublishParams{title}) = PublishParams::parse(query) {
                    match String::from_utf8(body_bytes) {
                        Ok(body) => {
                            page.set_title(title.unwrap_or("".to_string())).await;
                            page.set_body(body).await;
                        },
                        Err(_) => return Ok(bad_request("Invalid UTF-8 in POST data (only UTF-8 is supported).")),
                    };
                } else {
                    return Ok(bad_request("Invalid query string in POST."));
                }
            }

            // Give an empty response
            Response::new(Body::empty())
        },
        Method::DELETE => {
            if query != "" {
                return Ok(bad_request("Invalid query string in DELETE."));
            }
            // What are we doing here? We swap in an initial-state page for our
            // page, then force a client-side refresh to immediately clear the
            // client view. At some point in the future, if this page isn't
            // modified, and once all clients disconnect, it will be garbage
            // collected from the PAGES table by the heartbeat loop.
            let mut old_page = Page::new();
            mem::swap(&mut old_page, &mut page);
            old_page.refresh().await;
            Response::new(Body::empty())
        },
        Method::OPTIONS => {
            let options = if PublishParams::parse(query).is_some() {
                "OPTIONS, GET, HEAD, POST, DELETE"
            } else if RetrieveParams::parse(query).is_some() {
                "OPTIONS, GET, HEAD"
            } else {
                return Ok(bad_request("Invalid query string in OPTIONS."));
            };
            Response::builder()
                .header("Allow", options)
                .body(Body::empty())
                .unwrap()
        }
        _ => Response::builder()
            .status(StatusCode::METHOD_NOT_ALLOWED)
            .body(Body::empty())
            .unwrap(),
    };
    Ok(result)
}
