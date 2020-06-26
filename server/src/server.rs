use std::collections::HashMap;
use std::net::SocketAddr;
use tokio::time::Duration;
use std::sync::Arc;
use warp::{self, Filter, reject::Reject, Rejection};
use http::{Uri, Method, StatusCode};

use myxine::unique::Unique;
use myxine::session::{self, Session};
use myxine::page::{Page, RefreshMode, subscription::Subscription};

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

/// Handle a GET request, in the whole.
fn get(
    session: Arc<Session>
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    get_params()
        .and(page(session))
        .and_then(|params: params::GetParams, page: Arc<Page>| async move {
            let reply: Result<String, warp::Rejection> = Ok("GET".to_string());
            reply
            // match params {}
        })
}

/// Handle a POST request, in the whole.
fn post(
    session: Arc<Session>
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    post_params()
        .and(page(session))
        .and_then(|params: params::PostParams, page: Arc<Page>| async move {
            let reply: Result<String, warp::Rejection> = Ok("POST".to_string());
            reply
            // match params {}
        })
}

/// Handle a DELETE request, in the whole.
fn delete(
    session: Arc<Session>
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    no_params()
        .and(page(session))
        .and_then(|page: Arc<Page>| async move {
            let reply: Result<String, warp::Rejection> = Ok("DELETE".to_string());
            reply
        })
}

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
        get(session)
        .or(post(session))
        .or(delete(session))
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
