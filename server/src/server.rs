use std::net::SocketAddr;
use tokio::time::Duration;
use std::sync::Arc;
use hyper::body::Body;
use bytes::Bytes;
use warp::{self, Filter, reject::Reject, Rejection, path::FullPath};
use http::{Uri, StatusCode, Response};
use lazy_static::lazy_static;

use myxine::session::{self, Session};
use myxine::page::Page;

mod params;

/// The interval between heartbeats.
const HEARTBEAT_INTERVAL: Duration = Duration::from_secs(10);

/// The duration we should wait before marking a page as stale.
const KEEP_ALIVE_DURATION: Duration = Duration::from_secs(10);

/// The maximum size of the event buffer for each page: a consumer of events via
/// the polling interface can lag by this many events before dropping events.
const DEFAULT_BUFFER_LEN: usize = 1_000;

/// The default timeout for evaluating JavaScript expressions in the page,
/// measured in milliseconds
const DEFAULT_EVAL_TIMEOUT: Duration = Duration::from_secs(1);

/// A custom rejection for when the path ended with a slash
#[derive(Debug, Clone)]
struct Redirect(Uri);
impl Reject for Redirect {}

/// Get the `Page` in the given `Session` that corresponds to the path, or send
/// a `Rejection` that tells us to emit a redirect, if the path ends with a
/// slash.
fn page(
    session: Arc<Session>
) -> impl Filter<Extract = (Arc<Page>,), Error = warp::Rejection> + Clone {
    warp::path::full()
        .and_then(move |path: warp::path::FullPath| {
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
    warp::query::raw()
        .or(warp::any().map(|| String::new()))
        .unify()
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
    query().and_then(|params: String| async move {
        params::constrain_to_keys(params::query_params(&params), &[])
            .map_err(warp::reject::custom)
    }).untuple_one()
}

lazy_static! {
    /// The content of the dynamic page returned by default, including (inlined)
    /// all the JavaScript necessary to make it run.
    static ref DYNAMIC_PAGE: String =
        format!(
            include_str!("server/dynamic.html"),
            diff = include_str!("../deps/diffhtml.min.js"),
            dynamic = include_str!("server/dynamic.js"),
            enabled_events = include_str!("enabled-events.json"),
        );
}

/// Handle a GET request, in the whole.
fn get(
    session: Arc<Session>
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path::full()
        .and(get_params())
        .and(page(session))
        .and_then(|path: FullPath, params: params::GetParams, page: Arc<Page>| async move {
            use params::GetParams::*;
            use params::SubscribeParams::*;
            let mut response = Response::builder();
            Ok::<_, warp::Rejection>(match params {
                FullPage => {
                    if let Some((content_type, raw_contents)) = page.static_content().await {
                        if let Some(content_type) = content_type {
                            response = response.header("Content-Type", content_type);
                        }
                        response.body(raw_contents.into())
                    } else {
                        response.body(DYNAMIC_PAGE.as_str().into())
                    }
                },
                Subscribe { subscription, stream_or_after: Stream } =>
                    response.body(page.event_stream(subscription).await),
                Subscribe { subscription, stream_or_after: After(after) } =>
                    match page.event(subscription, after).await {
                        Ok((moment, body)) => {
                            response
                                .header("Content-Type", "application/json; charset=utf8")
                                .header("Content-Location", params::canonical_moment(&path, moment))
                                .body(body)
                        },
                        Err(moment) => {
                            response
                                .header("Location", params::canonical_moment(&path, moment))
                                .status(StatusCode::TEMPORARY_REDIRECT)
                                .body(format!("{}", moment - after).into())
                        }
                    },
                Subscribe { subscription, stream_or_after: Next } => {
                    let (moment, body) = page.next_event(subscription).await;
                    response
                        .header("Content-Type", "application/json; charset=utf8")
                        .header("Content-Location", params::canonical_moment(&path, moment))
                        .body(body)
                },
                // ----- INTERNAL API BELOW HERE -----
                PageUpdates => match page.update_stream().await {
                    Some(body) =>
                        response
                        .header("Content-Type", "text/event-stream")
                        .body(body),
                    None =>
                        response
                        .status(StatusCode::NOT_FOUND)
                        .body(Body::empty())
                },
            }.unwrap())
        })
}

/// Handle a POST request, in the whole.
fn post(
    session: Arc<Session>
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
                    Evaluate { expression, timeout } => {
                        match if let Some(expression) = expression {
                            page.evaluate(&expression, false, timeout).await
                        } else {
                            match std::str::from_utf8(bytes.as_ref()) {
                                Ok(statements) => page.evaluate(&statements, true, timeout).await,
                                Err(err) => {
                                    return Ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(format!("Invalid UTF-8 in POST data: {}", err).into())
                                        .unwrap());
                                }
                            }
                        } {
                            Ok(value) => {
                                let json = serde_json::to_string(&value).unwrap();
                                return Ok(Response::builder()
                                    .header("Content-Type", "application/json")
                                    .body(Body::from(json))
                                    .unwrap())
                            },
                            Err(err) => {
                                return Ok(Response::builder()
                                    .status(match err {
                                        myxine::page::EvalError::Timeout{..} => StatusCode::REQUEST_TIMEOUT,
                                        myxine::page::EvalError::NoBrowser => StatusCode::NOT_FOUND,
                                        myxine::page::EvalError::StaticPage => StatusCode::NOT_FOUND,
                                        myxine::page::EvalError::JsError{..} => StatusCode::BAD_REQUEST,
                                    })
                                    .body(format!("{}", err).into())
                                    .unwrap())
                            },
                        }
                    },
                    // ----- INTERNAL API BELOW HERE -----
                    PageEvent => {
                        match serde_json::from_slice(bytes.as_ref()) {
                            Ok(event) => page.send_event(event).await,
                            Err(err) => {
                                return Ok(Response::builder()
                                    .status(StatusCode::BAD_REQUEST)
                                    .body(format!("Invalid page event: {}", err).into())
                                    .unwrap());
                            },
                        }
                    },
                    EvalResult { id } => {
                        match serde_json::from_slice(bytes.as_ref()) {
                            Ok(result) => page.send_evaluate_result(id, Ok(result)).await,
                            Err(err) => {
                                let status = StatusCode::BAD_REQUEST;
                                let message = format!("Invalid JSON in evaluation result from browser: {}", err);
                                return Ok(Response::builder().status(status).body(message.into()).unwrap());
                            }
                        }
                    }
                    EvalError { id } => {
                        match std::str::from_utf8(bytes.as_ref()) {
                            Ok(error) => page.send_evaluate_result(id, Err(error.to_string())).await,
                            Err(err) => {
                                let status = StatusCode::BAD_REQUEST;
                                let message = format!("Invalid UTF-8 in evaluation error from browser: {}", err);
                                return Ok(Response::builder().status(status).body(message.into()).unwrap());
                            }
                        }
                    }
                };
                Response::new(Body::empty())
            })
        })
}

/// Handle a DELETE request, in the whole.
fn delete(
    session: Arc<Session>
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    no_params()
        .and(page(session))
        .and_then(|page: Arc<Page>| async move {
            page.clear().await;
            Ok::<_, warp::Rejection>("")
        })
}

/// The static string identifying the server and its major.minor version (we
/// don't reveal the patch because patch versions should not affect public API).
/// This allows clients to check whether they are compatible with this version
/// of the server.
const SERVER: &str =
    concat!(env!("CARGO_PKG_NAME"),
            "/",
            env!("CARGO_PKG_VERSION_MAJOR"),
            ".",
            env!("CARGO_PKG_VERSION_MINOR"));

pub(crate) async fn run(addr: impl Into<SocketAddr> + 'static) {
    // The session holding all the pages for this instantiation of the server
    let session = Arc::new(Session::start(session::Config {
        heartbeat_interval: HEARTBEAT_INTERVAL,
        keep_alive_duration: KEEP_ALIVE_DURATION,
        default_buffer_len: DEFAULT_BUFFER_LEN,
        default_eval_timeout: DEFAULT_EVAL_TIMEOUT,
    }).await);

    // Collect the routes
    let routes =
        get(session.clone())
        .or(post(session.clone()))
        .or(delete(session))
        .map(|reply| warp::reply::with_header(reply, "Cache-Control", "no-cache"))
        .map(|reply| warp::reply::with_header(reply, "Access-Control-Allow-Origin", "*"))
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
    warp::serve(routes).run(addr).await;
}
