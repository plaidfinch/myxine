use bytes::Bytes;
use futures::{FutureExt, SinkExt, StreamExt};
use http::{Response, StatusCode, Uri};
use hyper::body::Body;
use lazy_static::lazy_static;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::time::Duration;
use warp::{self, path::FullPath, reject::Reject, Filter, Rejection};

use myxine_core::{Page, Session};

use crate::params;

/// The interval between heartbeats.
const HEARTBEAT_INTERVAL: Duration = Duration::from_millis(100);

/// The duration we should wait before marking a page as stale.
const KEEP_ALIVE_DURATION: Duration = Duration::from_secs(1);

/// The maximum size of the event buffer for each page: a consumer of events via
/// the polling interface can lag by this many events before dropping events.
const DEFAULT_BUFFER_LEN: usize = 512;

/// A custom rejection for when the path ended with a slash
#[derive(Debug, Clone)]
struct Redirect(Uri);
impl Reject for Redirect {}

/// Get the `Page` in the given `Session` that corresponds to the path, or send
/// a `Rejection` that tells us to emit a redirect, if the path ends with a
/// slash.
fn page(
    session: Arc<Session>,
) -> impl Filter<Extract = (Arc<Page>,), Error = warp::Rejection> + Clone {
    warp::path::full().and_then(move |path: warp::path::FullPath| {
        let session = session.clone();
        async move {
            let path = path.as_str();
            let path_ends_with_slash = path != "/" && path.ends_with('/');
            let path = if path != "/" {
                path.trim_end_matches('/')
            } else {
                "/"
            };
            if path_ends_with_slash {
                Err(warp::reject::custom(Redirect(path.parse().unwrap())))
            } else {
                Ok(session.page(&path).await)
            }
        }
    })
}

/// Get the raw query string, if one is present, or the empty string if one is
/// not. This filter will never fail, unlike warp::query::raw, which fails if
/// there is not any query string.
fn query() -> impl Filter<Extract = (String,), Error = std::convert::Infallible> + Clone {
    warp::query::raw().or(warp::any().map(String::new)).unify()
}

/// If the request was a GET request, parse its query parameters as such, or
/// else fail.
fn get_params() -> impl Filter<Extract = (params::GetParams,), Error = warp::Rejection> + Clone {
    warp::get().and(query().and_then(|params: String| async move {
        params::GetParams::parse(&params).map_err(warp::reject::custom)
    }))
}

/// If the request was a POST request, parse its query parameters as such, or
/// else fail.
fn post_params() -> impl Filter<Extract = (params::PostParams,), Error = warp::Rejection> + Clone {
    warp::post().and(query().and_then(|params: String| async move {
        params::PostParams::parse(&params).map_err(warp::reject::custom)
    }))
}

/// Enforce that there are no query parameters, throwing a nice error if there
/// are any.
fn no_params() -> impl Filter<Extract = (), Error = warp::Rejection> + Clone {
    query()
        .and_then(|params: String| async move {
            params::constrain_to_keys(params::query_params(&params), &[])
                .map_err(warp::reject::custom)
        })
        .untuple_one()
}

lazy_static! {
    /// The content of the dynamic page returned by default, including (inlined)
    /// all the JavaScript necessary to make it run.
    static ref DYNAMIC_PAGE: String =
        format!(
            include_str!("dynamic.html"),
            diff = include_str!("../deps/diffhtml.min.js"),
            dynamic = include_str!("dynamic.js"),
            enabled_events = include_str!("enabled-events.json"),
        );
}

/// Handle a GET request, in the whole.
fn get(
    session: Arc<Session>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path::full()
        .and(get_params())
        .and(page(session))
        .and_then(
            |path: FullPath, params: params::GetParams, page: Arc<Page>| async move {
                use params::GetParams::*;
                use params::SubscribeParams::*;
                let mut response = Response::builder();
                Ok::<_, warp::Rejection>(
                    match params {
                        FullPage => {
                            if let Some((content_type, raw_contents)) = page.static_content().await
                            {
                                if let Some(content_type) = content_type {
                                    response = response.header("Content-Type", content_type);
                                }
                                response.body(raw_contents.into())
                            } else {
                                response.body(DYNAMIC_PAGE.as_str().into())
                            }
                        }
                        Connect => response
                            .header("Content-Type", "application/javascript; charset=utf8")
                            .body(include_str!("connect.js").into()),
                        Subscribe {
                            subscription,
                            stream_or_after: Stream,
                        } => {
                            let events = page.events(subscription).await;
                            let stream = hyper::body::Body::wrap_stream(events.map(|event| {
                                let mut line = serde_json::to_vec(&*event).unwrap();
                                line.push(b'\n');
                                Ok::<Vec<u8>, std::convert::Infallible>(line)
                            }));
                            response.body(stream)
                        }
                        Subscribe {
                            subscription,
                            stream_or_after: After(after),
                        } => match page.event_after(subscription, after).await {
                            Ok((moment, event)) => response
                                .header("Content-Type", "application/json; charset=utf8")
                                .header("Content-Location", params::canonical_moment(&path, moment))
                                .body(serde_json::to_vec(&*event).unwrap().into()),
                            Err(moment) => response
                                .header("Location", params::canonical_moment(&path, moment))
                                .status(StatusCode::TEMPORARY_REDIRECT)
                                .body(format!("{}", moment - after).into()),
                        },
                        Subscribe {
                            subscription,
                            stream_or_after: Next,
                        } => {
                            let (moment, event) = page.next_event(subscription).await;
                            response
                                .header("Content-Type", "application/json; charset=utf8")
                                .header("Content-Location", params::canonical_moment(&path, moment))
                                .body(serde_json::to_vec(&*event).unwrap().into())
                        }
                    }
                    .unwrap(),
                )
            },
        )
}

/// Handle a POST request, in the whole.
fn post(
    session: Arc<Session>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    post_params()
        .and(page(session))
        .and(warp::header::optional::<String>("Content-Type"))
        .and(warp::body::bytes())
        .and_then(|params: params::PostParams, page: Arc<Page>, mut content_type: Option<String>, bytes: Bytes| async move {
            use params::PostParams::*;
            // One of these content types means it's just data, we know nothing
            // about it, and we should just serve it up as a unicode string. The
            // user should specify some particular content type if they desire
            // one.
            if let Some(ref t) = content_type {
                if t.starts_with("application/x-www-form-urlencoded")
                    || t.starts_with("multipart/form-data")
                {
                    content_type = None;
                }
            }
            Ok::<_, warp::Rejection>({
                match params {
                    StaticPage => page.set_static(content_type, bytes).await,
                    DynamicPage { title, refresh } => {
                        match std::str::from_utf8(bytes.as_ref()) {
                            Ok(body) => page.set_content(title, body, refresh).await,
                            Err(err) => {
                                return Ok(Response::builder()
                                    .status(StatusCode::BAD_REQUEST)
                                    .body(format!("Invalid UTF-8 in POST data: {}", err).into())
                                    .unwrap());
                            },
                        }
                    },
                    Evaluate { expression } => {
                        // Make an "abort" future that will complete if this
                        // enclosing future is dropped: that is, if the client
                        // kills the connection, regardless of whether we've
                        // gotten an evaluation result, we're done.
                        let (_tx, rx) = tokio::sync::oneshot::channel::<()>();
                        let abort = rx.map(|_| ());
                        return tokio::spawn(async move {
                            match if let Some(expression) = expression {
                                if bytes.is_empty() {
                                    page.evaluate(&expression, false, abort).await
                                } else {
                                    return Ok(Response::builder()
                                              .status(StatusCode::BAD_REQUEST)
                                              .body("Expecting empty body with non-empty ?evaluate=... query parameter".into())
                                              .unwrap())
                                }
                            } else {
                                match std::str::from_utf8(bytes.as_ref()) {
                                    Ok(statements) => page.evaluate(&statements, true, abort).await,
                                    Err(err) => {
                                        return Ok(Response::builder()
                                                  .status(StatusCode::BAD_REQUEST)
                                                  .body(format!("Invalid UTF-8 in POST data: {}", err).into())
                                                  .unwrap());
                                    }
                                }
                            } {
                                Some(Ok(value)) => {
                                    let json = serde_json::to_string(&value).unwrap();
                                    Ok(Response::builder()
                                       .header("Content-Type", "application/json")
                                       .body(Body::from(json))
                                       .unwrap())
                                },
                                Some(Err(err)) => {
                                    Ok(Response::builder()
                                       .status(StatusCode::BAD_REQUEST)
                                       .body(err.into())
                                       .unwrap())
                                },
                                None => {
                                    Ok(Response::builder()
                                       .status(StatusCode::INTERNAL_SERVER_ERROR)
                                       .body("JavaScript evaluation aborted before connection closed".into())
                                       .unwrap())
                                },
                            }
                        }).await.unwrap()
                    },
                };
                Response::new(Body::empty())
            })
        })
}

/// Handle a DELETE request, in the whole.
fn delete(
    session: Arc<Session>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    no_params()
        .and(page(session))
        .and_then(|page: Arc<Page>| async move {
            page.clear().await;
            Ok::<_, warp::Rejection>("")
        })
}

/// Handle a websocket upgrade from the page.
fn websocket(
    session: Arc<Session>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::ws()
        .and(page(session))
        .map(|ws: warp::ws::Ws, page: Arc<Page>| {
            ws.on_upgrade(|websocket| async {
                // Forward page updates to the page
                let (mut tx, mut rx) = websocket.split();
                if let Some(mut commands) = page.commands().await {
                    tokio::spawn(async move {
                        while let Some(command) = commands.next().await {
                            let message = serde_json::to_string(&command).unwrap();
                            if tx.send(warp::ws::Message::text(message)).await.is_err() {
                                break;
                            }
                        }
                    });
                }
                // Forward responses from the browser to their handlers
                tokio::spawn(async move {
                    while let Some(Ok(message)) = rx.next().await {
                        if let Ok(text) = message.to_str() {
                            if let Ok(response) = serde_json::from_str(text) {
                                match response {
                                    myxine_core::Response::Event(event) => {
                                        page.send_event(event).await
                                    }
                                    myxine_core::Response::EvalResult { id, result } => {
                                        page.send_eval_result(id, result).await
                                    }
                                }
                            } else {
                                // Couldn't parse response.
                                break;
                            }
                        }
                    }
                });
            })
        })
}

/// The static string identifying the server and its major.minor version (we
/// don't reveal the patch because patch versions should not affect public API).
/// This allows clients to check whether they are compatible with this version
/// of the server.
const SERVER: &str = concat!(
    env!("CARGO_PKG_NAME"),
    "/",
    env!("CARGO_PKG_VERSION_MAJOR"),
    ".",
    env!("CARGO_PKG_VERSION_MINOR")
);

pub(crate) async fn run(addr: impl Into<SocketAddr> + 'static) -> Result<(), warp::Error> {
    // The session holding all the pages for this instantiation of the server
    let session = Arc::new(
        Session::start(myxine_core::Config {
            heartbeat_interval: HEARTBEAT_INTERVAL,
            keep_alive_duration: KEEP_ALIVE_DURATION,
            default_buffer_len: DEFAULT_BUFFER_LEN,
        })
        .await,
    );

    // Collect the routes
    let routes = websocket(session.clone())
        .or(get(session.clone()))
        .or(post(session.clone()))
        .or(delete(session))
        .map(|reply| warp::reply::with_header(reply, "Cache-Control", "no-cache"))
        .map(|reply| warp::reply::with_header(reply, "Server", SERVER))
        .recover(|err: Rejection| async {
            if let Some(Redirect(uri)) = err.find() {
                Ok(warp::redirect(uri.clone()))
            } else {
                Err(err)
            }
        })
        .recover(|err: Rejection| async {
            if let Some(param_error) = err.find::<params::ParseError>() {
                Ok(warp::reply::with_status(
                    format!("{}", param_error),
                    StatusCode::BAD_REQUEST,
                ))
            } else {
                Err(err)
            }
        });

    // Run the server
    match warp::serve(routes).try_bind_ephemeral(addr) {
        Ok((actual_addr, server)) => {
            println!("http://{}", actual_addr);
            server.await;
            Ok(())
        }
        Err(err) => Err(err),
    }
}
