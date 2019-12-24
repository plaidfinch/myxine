#![feature(async_closure)]
use futures::stream::StreamExt;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, StatusCode, Uri};
use http::request::Parts;
use std::sync::Arc;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::mem;
// use std::time::Duration;
use tokio::sync::Mutex;
// use tokio::time;

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

async fn process_request(
    base_uri: Arc<Uri>,
    pages: Arc<Mutex<HashMap<String, Page>>>,
    request: Request<Body>,
) -> Result<Response<Body>, hyper::Error> {

    // Disassemble the request into the parts we care about
    let (parts, mut body) = request.into_parts();

    // More disassembly
    let Parts{method, uri, ..} = parts;
    let query = uri.query().unwrap_or("");
    let path = uri.path().to_string();

    // Get (or create) the page at this path
    let mut pages = pages.lock().await;
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
        Method::PUT | Method::POST | Method::PATCH => {
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
                            Ok(body) if method != Method::PATCH =>
                                page.set_body(body).await,
                            Ok(_) => { },
                            Err(_) => return Ok(bad_request("Invalid UTF-8.")),
                        };
                        match title {
                            None if method != Method::PATCH =>
                                page.set_title("").await,
                            Some(title) =>
                                page.set_title(title).await,
                            _ => { },
                        }
                    },
                }
            } else {
                return Ok(bad_request("Invalid query string in PUT/POST/PATCH."));
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
                "OPTIONS, GET, HEAD, POST, PATCH, PUT, DELETE"
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

#[tokio::main]
async fn main() {
    let host = [127, 0, 0, 1];
    let port = 8000;

    let socket_addr = (host, port).into();
    let base_uri = Uri::try_from(format!("http://{}", socket_addr)).unwrap();

    let pages = Arc::new(Mutex::new(HashMap::new()));
    let base_uri = Arc::new(base_uri);

    // TODO: background task to keep connections alive

    let service = make_service_fn(move |_| {
        let pages = pages.clone();
        let base_uri = base_uri.clone();
        async {
            Ok::<_, hyper::Error>(service_fn(move |request: Request<Body>| {
                process_request(base_uri.clone(), pages.clone(), request)
            }))
        }
    });

    let server = hyper::Server::bind(&socket_addr);

    if let Err(err) = server.serve(service).await {
        eprintln!("Server error: {}", err);
    }
}
