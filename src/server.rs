use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, StatusCode};
use hyper::server::conn::AddrStream;
use http::request::Parts;
use http::header::{HeaderMap, HeaderValue};
use lazy_static::lazy_static;
use std::sync::Arc;
use std::collections::{hash_map::Entry, HashMap};
use std::net::SocketAddr;
use tokio::sync::Mutex;
use futures::future::FutureExt;
use futures::{future, select, pin_mut};
use futures::stream::StreamExt;
use itertools::Itertools;
use void::Void;

mod params;
mod heartbeat;

use crate::page::Page;
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
        (Method::GET, _) =>
            Response::builder().status(StatusCode::NOT_FOUND).body(Body::empty()).unwrap(),
        _ => Response::builder().status(StatusCode::METHOD_NOT_ALLOWED).body(Body::empty()).unwrap(),
    })
}

// The location of the special path for myxine's assets
// (this path and its subpaths cannot be treated as ordinary
const MYXINE_SPECIAL_PATH: &str = "/.myxine/";

async fn process_request(request: Request<Body>) -> Result<Response<Body>, hyper::Error> {

    // Disassemble the request into the parts we care about
    let (parts, mut body) = request.into_parts();

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

        Method::GET | Method::HEAD => {
            let mut body = Body::empty();
            match GetParams::parse(query) {
                // If client wants event stream of changes to page:
                Some(GetParams::PageUpdates) => {
                    if method == Method::GET {
                        body = page.update_stream().await.unwrap_or_else(Body::empty);
                    }
                    Response::builder()
                        .header("Content-Type", "text/event-stream")
                        .header("Cache-Control", "no-cache")
                        .header("Access-Control-Allow-Origin", "*")
                        .body(body)
                        .unwrap()
                },
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
                    if method == Method::GET {
                        body = page.render().await.into();
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
            // TODO: delay this until we need to do it by refactoring out into a
            // separate function?
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
                        page.set_static(content_type.map(String::from), body_bytes).await;
                        Response::new(Body::empty())
                    } else {
                        Response::builder()
                            .status(StatusCode::FORBIDDEN)
                            .body(Body::empty())
                            .unwrap()
                    }
                },
                // Client wants to publish some HTML to a dynamic page:
                Some(PostParams::DynamicPage{title}) => {
                    if writeable_path {
                        match String::from_utf8(body_bytes) {
                            Ok(body) => {
                                if cfg!(debug_assertions) {
                                    eprintln!("\n{}", body);
                                }
                                page.set_title(title).await;
                                page.set_body(body).await;
                                Response::new(Body::empty())
                            },
                            Err(_) =>
                                return Ok(bad_request("Invalid UTF-8 in POST data (only UTF-8 is supported).")),
                        }
                    } else {
                        Response::builder()
                            .status(StatusCode::FORBIDDEN)
                            .body(Body::empty())
                            .unwrap()
                    }
                },
                // Client wants to subscribe to interface events on this page:
                Some(PostParams::SubscribeEvents{uuid}) => {
                    if let Ok(subscription) = serde_json::from_slice(&body_bytes) {
                        if cfg!(debug_assertions) {
                            eprintln!("\n{:?}", &subscription);
                        }
                        if let Some((uuid, body)) =
                            page.event_stream(uuid, subscription).await
                        {
                            let content_location =
                                format!("{}?resubscribe={}", path, uuid.to_simple_ref());
                            Response::builder()
                                .header("Content-Type", "text/event-stream")
                                .header("Cache-Control", "no-cache")
                                .header("Access-Control-Allow-Origin", "*")
                                .header("Content-Location", content_location)
                                .body(body)
                                .unwrap()
                        } else {
                            return Ok(bad_request("Invalid resubscription request: stream does not exist."))
                        }
                    } else {
                        return Ok(bad_request("Invalid subscription request."));
                    }
                },
                // Browser wants to notify client of an event
                Some(PostParams::PageEvent{event, target, id}) => {
                    if let Ok(event_data) = serde_json::from_slice(&body_bytes) {
                        tokio::spawn(async move {
                            page.send_event(&event, &target, &id, &event_data).await;
                        });
                        Response::new(Body::empty())
                    } else {
                        return Ok(bad_request("Invalid page event."));
                    }
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
