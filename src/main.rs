#![feature(async_closure)]
use futures::{future, join, stream::StreamExt};
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, StatusCode, Uri};
use http::request::Parts;
use lazy_static::lazy_static;
use std::sync::Arc;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::mem;
use std::iter::Iterator;
use std::time::Duration;
use tokio::sync::Mutex;
use tokio::time;

mod params;
mod page;
use params::{RetrieveParams, PublishParams};
use page::Page;

fn bad_request(message: impl Into<String>) -> Response<Body> {
    Response::builder()
        .status(StatusCode::BAD_REQUEST)
        .body(Body::from(message.into()))
        .unwrap()
}

/// Send a heartbeat message to keep all page connections alive, simultaneously
/// pruning all pages from memory which have no content and no subscribers.
async fn heartbeat() {
    let mut pages = PAGES.lock().await;
    // Determine which pages to delete, in the process sending heartbeats.
    let garbage: Vec<Option<String>> =
        future::join_all(
            pages.iter_mut().map(|(path, page)| async move {
                // Send a heartbeat message to each page and count the number of
                // still-extant connections
                let connected = page.heartbeat().await;
                match page {
                    Page::Dynamic{title, body, ..}
                    // We'll delete all the entries for pages for which:
                    //   1) there are no connected clients, and
                    //   2) the contents of the page are empty
                    // That is to say, pages for which we need not preserve any
                    // information to reconstruct them again on next load.
                    if connected == 0 && title == "" && body == "" =>
                        Some(path.clone()),
                    _ => None
                }
            })).await;
    // Actually collect the garbage, removing all pages slated for deletion.
    for path in garbage {
        path.map(|path| pages.remove(&path));
    }
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
    let mut page = pages.entry(path.clone()).or_insert(Page::new());

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
            let mut temp_page = Page::new();
            mem::swap(&mut temp_page, &mut page);
            pages.remove(&path);
            temp_page.refresh().await;
            Response::new(Body::empty())
        },
        Method::OPTIONS => {
            let options = if PublishParams::parse(query).is_some() {
                "OPTIONS, GET, HEAD, POST, DELETE"
            } else if RetrieveParams::parse(query).is_some() {
                "OPTIONS, GET, HEAD"
            } else {
                return Ok(bad_request("Invalid query string."));
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

const HOST: [u8; 4] = [127, 0, 0, 1];
const PORT: u16 = 8000;
const HEARTBEAT_INTERVAL: u64 = 10;

lazy_static! {
    static ref PAGES: Mutex<HashMap<String, Page>>
        = Mutex::new(HashMap::new());
}

#[tokio::main]
async fn main() {
    let socket_addr = (HOST, PORT).into();
    let base_uri =
        Arc::new(Uri::try_from(format!("http://{}", socket_addr)).unwrap());

    // The main server future
    let server = async {
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
            // TODO: Signal quit condition here
            eprintln!("{}", err);
        })
    };

    // TODO: a no-activity timeout future

    // The heartbeat / garbage collector future
    let heartbeat = async {
        tokio::spawn(async {
            loop {
                time::delay_for(Duration::from_secs(HEARTBEAT_INTERVAL)).await;
                heartbeat().await;
            }
        }).await.unwrap();
    };

    join!(server, heartbeat);
}
