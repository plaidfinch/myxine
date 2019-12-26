use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, StatusCode, Uri};
use http::request::Parts;
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

    // More disassembly
    let Parts{method, uri, ..} = parts;
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
            if let Some(params) = RetrieveParams::parse(query) {
                if !params.stream {
                    // Client wants entire full page
                    let event_stream_uri =
                        base_uri.to_string().trim_end_matches("/").to_owned()
                        + &path + "?stream=true";
                    let body = if method == Method::HEAD {
                        Body::empty()
                    } else {
                        page.render(&event_stream_uri).into()
                    };
                    Response::builder()
                        .header("Cache-Control", "no-cache")
                        .body(body)
                        .unwrap()
                } else {
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
            // Dispatch on the query parameters
            if let Some(params) = PublishParams::parse(query) {
                match params {
                    PublishParams::Static => {
                        page.set_static(body_bytes).await;
                    },
                    PublishParams::Dynamic{title} => {
                        match String::from_utf8(body_bytes) {
                            Ok(body) => {
                                page.set_title(title.unwrap_or("")).await;
                                page.set_body(body).await;
                            },
                            Err(_) => return Ok(bad_request("Invalid UTF-8.")),
                        };
                    },
                }
            } else {
                return Ok(bad_request("Invalid query string in POST."));
            }
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
