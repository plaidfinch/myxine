use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, StatusCode};
use hyper::server::conn::AddrStream;
use hyper::body;
use http::request::Parts;
use http::header::{HeaderMap, HeaderValue};
use lazy_static::lazy_static;
use std::sync::Arc;
use std::collections::{hash_map::Entry, HashMap};
use std::net::SocketAddr;
use tokio::sync::Mutex;
use futures::future::FutureExt;
use futures::{future, select, pin_mut};
use itertools::Itertools;
use void::Void;

mod params;
mod heartbeat;

use crate::page::{Page, subscription::Event};
use params::{GetParams, PostParams};

lazy_static! {
    /// The current contents of the server, indexed by path
    pub(crate) static ref PAGES:
    Mutex<HashMap<String, Arc<Page>>>
        = Mutex::new(HashMap::new());
}

/// Run the main server loop alongside the heartbeat to all SSE clients
#[allow(clippy::unnecessary_mut_passed)]
pub async fn run(socket_addr: SocketAddr) -> Result<(), hyper::Error> {

    // The regular server
    let server = hyper::Server::try_bind(&socket_addr)?
        .serve(make_service_fn(|_socket: &AddrStream| {
            async { Ok::<_, Void>(service_fn(process_request)) }
        }));

    // The heartbeat loop
    let heartbeat = heartbeat::heartbeat_loop().fuse();

    // Note the local address to the user
    println!("Running at: http://{}",
             server.local_addr().to_string().trim_end_matches('/'));

    // Signals to shut down the server
    let serve = server.with_graceful_shutdown(async {
        tokio::signal::ctrl_c().await.unwrap_or(());
        // Clean-up after the server is killed: set every extant page to the
        // empty page and remove all their subscriptions
        let mut pages = PAGES.lock().await;
        let all_pages = pages.values().cloned().collect::<Vec<_>>();
        pages.clear();
        future::join_all(all_pages.iter().map(move |page| page.clear())).await;
    }).fuse();

    // Run the heartbeat loop and server concurrently
    pin_mut!(serve, heartbeat);
    select! {
        result = serve => result,
        () = heartbeat => Ok(()),
    }
}

/// Get a page from the global table (creating it if it does not yet exist) and
/// make sure that it receives heartbeats in the future
async fn get_page(path: &str) -> Arc<Page> {
    // Make sure this path receives heartbeats
    heartbeat::hold_path(path.to_string());

    // Get (or create) the page at this path. Note that this does *not* hold the
    // lock on PAGES, but rather extracts a clone of the `Arc<Mutex<Page>>` at
    // this path.
    match PAGES.lock().await.entry(path.to_string()) {
        Entry::Vacant(e) => {
            let page = Arc::new(Page::new().await);
            e.insert(page.clone());
            page
        },
        Entry::Occupied(e) => e.get().clone(),
    }
}

/// The response for serving a particular file as a static asset with liberal
/// cache policy (if specified) and a particular content type.
macro_rules! static_asset {
    ($cache:expr, $content_type:expr, $path:expr) => {
        {
            let mut builder = Response::builder()
                .header("Access-Control-Allow-Origin", "*")
                .header("Content-Type", $content_type);
            if $cache {
                builder = builder.header("Cache-Control", "public, max-age=31536000");
            }
            builder.body(Body::from(include_str!($path))).unwrap()
        }
    };
}

/// Process requests specific to the special '/.myxine/' path (the only path
/// which is not useable as a normal endpoint). This is used for ser
fn process_special_request(
    method: Method, path: &str, _query: &str
) -> Result<Response<Body>, hyper::Error> {
    Ok(match (method, path) {
        (Method::GET, "/assets/diffhtml.min.js") =>
            static_asset!(true, "application/javascript", "server/assets/diffhtml.min.js"),
        (Method::GET, "/assets/dynamic-page.js") =>
            static_asset!(false, "application/javascript", "server/assets/dynamic-page.js"),
        (Method::GET, "/assets/post.js") =>
            static_asset!(false, "application/javascript", "server/assets/post.js"),
        (Method::GET, "/assets/enabled-events.json") =>
            static_asset!(false, "application/json", "enabled-events.json"),
        (Method::GET, _) =>
            response_with_status(StatusCode::NOT_FOUND, "Page not found."),
        _ => response_with_status(StatusCode::METHOD_NOT_ALLOWED, "Method not allowed."),
    })
}

// The location of the special path for myxine's assets
// (this path and its subpaths cannot be treated as ordinary
const MYXINE_SPECIAL_PATH: &str = "/.myxine/";

async fn process_request(request: Request<Body>) -> Result<Response<Body>, hyper::Error> {

    // Disassemble the request into the parts we care about
    let (parts, body) = request.into_parts();

    // More disassembly
    let Parts{method, uri, headers, ..} = parts;
    let query = uri.query().unwrap_or("");

    // The path should be trimmed so it does not end with slashes. This is to
    // prevent confusion where /some/path/ contains different content than
    // /some/path, which is unintuitive if you think of paths as being composed
    // of hierarchical directories.
    let path = uri.path().to_string();
    let path_ends_with_slash = path != "/" && path.ends_with('/');
    let path = if path != "/" {
        path.trim_end_matches('/')
    } else {
        "/"
    };

    if cfg!(debug_assertions) {
        // Diagnostics about the requests we're receiving
        eprintln!("\n{} {}", method, uri);
        eprint_header(&headers, "Accept");
        eprint_header(&headers, "Content-Type");
    }

    // Shortcut to special processing if the path is in our reserved namespace
    if path.starts_with(MYXINE_SPECIAL_PATH) {
        let subpath = &path[MYXINE_SPECIAL_PATH.len() - 1 ..];
        return process_special_request(method, subpath, query);
    }

    // Get the page at this path
    let page = get_page(&path).await;

    // Just one big dispatch on the HTTP method...
    Ok(match method {

        Method::GET => {
            match GetParams::parse(query) {
                // Browser wants event stream of changes to page:
                Some(GetParams::PageUpdates) => {
                    let body = page.update_stream().await.unwrap_or_else(Body::empty);
                    Response::builder()
                        .header("Content-Type", "text/event-stream")
                        .header("Cache-Control", "no-cache")
                        .header("Access-Control-Allow-Origin", "*")
                        .body(body)
                        .unwrap()
                },
                // Client wants to subscribe to events on this page:
                Some(GetParams::Subscribe(subscription)) => {
                    let body = page.event_stream(subscription).await;
                    Response::builder()
                        .header("Content-Type", "text/event-stream")
                        .header("Cache-Control", "no-cache")
                        .header("Access-Control-Allow-Origin", "*")
                        .body(body)
                        .unwrap()
                },
                // Browser wants to load the full page
                Some(GetParams::FullPage) => {
                    let mut builder = Response::builder()
                        .header("Access-Control-Allow-Origin", "*")
                        .header("Content-Disposition", "inline");
                    if let Some(content_type) = page.content_type().await {
                        // If there's a custom content-type, set it here
                        builder = builder.header("Content-Type", content_type);
                    }
                    // We want to redirect to paths without slashes at the end
                    if path_ends_with_slash {
                        builder = builder.status(StatusCode::MOVED_PERMANENTLY);
                        builder = builder.header("Location", path);
                    } else {
                        // 301 redirects can be cached, but nothing else can
                        builder = builder.header("Cache-Control", "no-cache");
                    }
                    let body = page.render().await.into();
                    builder.body(body).unwrap()
                },
                None => {
                    response_with_status(StatusCode::BAD_REQUEST, "Invalid query string in GET.")
                },
            }
        },

        Method::POST => {
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
                        return Ok(response_with_status(
                            StatusCode::BAD_REQUEST,
                            "Invalid ASCII in Content-Type header."
                        )),
                };

            // Determine if the endpoint is a "special" path, which cannot be
            // updated by the user

            // TODO: implement dashboard at special path root. Instead of
            // unilaterally banning updates to the path, we should allow them
            // only if a query parameter matches a dynamically generated token
            // which is never revealed outside the process, which means only we
            // ourselves can update the page, but we can do so through the
            // public-facing API using the client library, rather than fiddling
            // with the Page contents directly. This also has the side-effect of
            // forcing us to eat our own dogfood, to construct this page.
            let writeable_path =
                path != MYXINE_SPECIAL_PATH.trim_end_matches('/')
                && !path.starts_with(MYXINE_SPECIAL_PATH);

            match PostParams::parse(query) {
                // Client wants to store a static file of a known Content-Type:
                Some(PostParams::StaticPage) => {
                    if writeable_path {
                        page.set_static(content_type.map(String::from),
                                        body::to_bytes(body).await?).await;
                        Response::new(Body::empty())
                    } else {
                        response_with_status(
                            StatusCode::FORBIDDEN,
                            "Clients cannot set the contents of the dashboard page.",
                        )
                    }
                },
                // Client wants to publish some HTML to a dynamic page:
                Some(PostParams::DynamicPage{title}) => {
                    if writeable_path {
                        let body_bytes = body::to_bytes(body).await?.as_ref().into();
                        match String::from_utf8(body_bytes) {
                            Ok(body) => {
                                if cfg!(debug_assertions) {
                                    eprintln!("\n{}", body);
                                }
                                page.set_title(title).await;
                                page.set_body(body).await;
                                Response::new(Body::empty())
                            },
                            Err(_) => response_with_status(
                                StatusCode::BAD_REQUEST,
                                "Invalid UTF-8 in POST data (only UTF-8 is supported).",
                            ),
                        }
                    } else {
                        response_with_status(
                            StatusCode::FORBIDDEN,
                            "Clients cannot set the contents of the dashboard page.",
                        )
                    }
                },
                // Browser wants to notify client of an event
                Some(PostParams::PageEvent) => {
                    match serde_json::from_slice(body::to_bytes(body).await?.as_ref()) {
                        Ok(Event{event, id, data}) => {
                            page.send_event(&event, &id, &data).await;
                            Response::new(Body::empty())
                        },
                        Err(err) => {
                            response_with_status(
                                StatusCode::BAD_REQUEST,
                                format!("Invalid page event: {}.", err),
                            )
                        }
                    }
                },
                // Client wants to evaluate a JavaScript expression in the
                // context of the browser
                Some(PostParams::Evaluate{expression, timeout}) => {
                    let result = if let Some(expression) = expression {
                        page.evaluate(&expression, false, timeout).await
                    } else {
                        let body_bytes = body::to_bytes(body).await?.as_ref().into();
                        match String::from_utf8(body_bytes) {
                            Ok(statements) => {
                                page.evaluate(&statements, true, timeout).await
                            },
                            Err(_) => return Ok(response_with_status(
                                StatusCode::BAD_REQUEST,
                                "Invalid UTF-8 in POST data (only UTF-8 is supported).",
                            )),
                        }
                    };
                    match result {
                        Err(err) => {
                            response_with_status(StatusCode::BAD_REQUEST, err)
                        },
                        Ok(value) => {
                            Response::new(Body::from(
                                serde_json::to_string(&value).expect("JSON encoding shouldn't fail")
                            ))
                        },
                    }
                },
                // Browser wants to notify client of successful evaluation result
                Some(PostParams::EvalResult{id}) => {
                    let body_bytes = body::to_bytes(body).await?;
                    match serde_json::from_slice(body_bytes.as_ref()) {
                        Ok(result) => {
                            page.send_evaluate_result(id, Ok(result)).await;
                            Response::new(Body::empty())
                        },
                        Err(err) => {
                            let status = StatusCode::INTERNAL_SERVER_ERROR;
                            let message = format!("Internal error: Invalid JSON in evaluation result from browser: {}", err);
                            response_with_status(status, message)
                        },
                    }
                },
                // Browser wants to notify client of erroneous evaluation result
                Some(PostParams::EvalError{id}) => {
                    let body_bytes = body::to_bytes(body).await?.as_ref().into();
                    match String::from_utf8(body_bytes) {
                        Ok(error) => {
                            page.send_evaluate_result(id, Err(error)).await;
                            Response::new(Body::empty())
                        },
                        Err(_) => {
                            let status = StatusCode::INTERNAL_SERVER_ERROR;
                            let message = "Internal error: Invalid UTF-8 in evaluation error from browser";
                            response_with_status(status, message)
                        },
                    }
                },
                None => {
                    return Ok(response_with_status(
                        StatusCode::BAD_REQUEST,
                        "Invalid query string in POST.",
                    ));
                }
            }
        },

        Method::DELETE => {
            // This is equivalent to an empty-body POST with no query string
            if query != "" {
                return Ok(response_with_status(StatusCode::BAD_REQUEST, "Invalid query string in DELETE."));
            }
            page.set_title("").await;
            page.set_body("").await;
            Response::new(Body::empty())
        },

        _ => Response::builder()
            .status(StatusCode::METHOD_NOT_ALLOWED)
            .body(format!("Method not allowed: {}", method).into())
            .unwrap(),
    })
}

// Some utilities for this module:

/// Construct a response with a given status code
fn response_with_status(status: StatusCode, body: impl Into<Body>) -> Response<Body> {
    Response::builder().status(status).body(body.into()).unwrap()
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