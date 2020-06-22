use serde_urlencoded;
use std::collections::{HashMap, HashSet};
use std::time::Duration;

use myxine::page::Subscription;
use myxine::unique::Unique;
use myxine::page::RefreshMode;

/// Parsed parameters from a query string for a GET/HEAD request.
pub enum GetParams {
    FullPage,
    PageUpdates,
    Subscribe {
        subscription: Subscription,
        stream_or_after: SubscribeParams,
    },
}

/// The manner in which a subscription should be handled.
pub enum SubscribeParams {
    /// Stream all events to the client.
    Stream,
    /// Return the earliest event after the given moment to the client, as soon
    /// as it is available.
    After(u64),
    /// Return the next event matching the subscription, but don't return any
    /// events that have already been registered.
    Next,
}

impl GetParams {
    /// Parse a query string from a GET request.
    pub fn parse(query: &str) -> Option<GetParams> {
        let params = query_params(&query)?;
        if constrained_to_keys(&params, &[]) {
            Some(GetParams::FullPage)
        } else if param_as_flag("updates", &params)? && constrained_to_keys(&params, &["updates"]) {
            Some(GetParams::PageUpdates)
        } else if param_as_flag("stream", &params)? {
            if constrained_to_keys(&params, &["events", "event", "stream"]) {
                Some(GetParams::Subscribe {
                    subscription: parse_subscription(&params),
                    stream_or_after: SubscribeParams::Stream,
                })
            } else {
                None
            }
        } else if constrained_to_keys(&params, &["events", "event", "after", "next"]) {
            if let Some(after_str) = param_as_str("after", &params) {
                let after = u64::from_str_radix(after_str, 10).ok()?
                    + if param_as_flag("next", &params)? {
                        1
                    } else {
                        0
                    };
                Some(GetParams::Subscribe {
                    subscription: parse_subscription(&params),
                    stream_or_after: SubscribeParams::After(after),
                })
            } else {
                Some(GetParams::Subscribe {
                    subscription: parse_subscription(&params),
                    stream_or_after: SubscribeParams::Next,
                })
            }
        } else {
            None
        }
    }
}

fn parse_subscription<'a>(params: &'a HashMap<String, Vec<String>>) -> Subscription {
    let events = match (
        param_as_strs("events", params),
        param_as_strs("event", params),
    ) {
        (Some(e1), Some(e2)) => e1.chain(e2).map(String::from).collect(),
        (Some(e1), None) => e1.map(String::from).collect(),
        (None, Some(e2)) => e2.map(String::from).collect(),
        (None, None) => HashSet::new(),
    };
    if events.is_empty() {
        Subscription::universal()
    } else {
        Subscription::from_events(events)
    }
}

/// Parsed parameters from a query string for a POST request.
pub(crate) enum PostParams {
    DynamicPage {
        title: String,
        refresh: RefreshMode,
    },
    StaticPage,
    Evaluate {
        expression: Option<String>,
        timeout: Option<Duration>,
    },
    // TODO: Add validation key to page-sent events to prevent MITM?
    PageEvent,
    EvalResult {
        id: Unique,
    },
    EvalError {
        id: Unique,
    },
}

impl PostParams {
    /// Parse a query string from a POST request.
    pub fn parse(query: &str) -> Option<PostParams> {
        let params = query_params(query)?;
        if constrained_to_keys(&params, &["title", "refresh"]) {
            let title = param_as_str("title", &params).unwrap_or("").to_string();
            let refresh = match param_as_flag("refresh", &params) {
                Some(true) => RefreshMode::FullReload,
                Some(false) => RefreshMode::Diff,
                None => match param_as_str("refresh", &params)? {
                    "full" => RefreshMode::FullReload,
                    "set" => RefreshMode::SetBody,
                    "diff" => RefreshMode::Diff,
                    _ => return None,
                },
            };
            return Some(PostParams::DynamicPage { title, refresh });
        } else if constrained_to_keys(&params, &["static"]) {
            if param_as_flag("static", &params)? {
                return Some(PostParams::StaticPage);
            }
        } else if constrained_to_keys(&params, &["page-result"]) {
            let id = Unique::parse_str(param_as_str("page-result", &params)?)?;
            return Some(PostParams::EvalResult { id });
        } else if constrained_to_keys(&params, &["page-error"]) {
            let id = Unique::parse_str(param_as_str("page-error", &params)?)?;
            return Some(PostParams::EvalError { id });
        } else if constrained_to_keys(&params, &["page-event"]) {
            if param_as_flag("page-event", &params)? {
                return Some(PostParams::PageEvent);
            }
        } else if constrained_to_keys(&params, &["evaluate", "timeout"]) {
            let timeout = if let Some(timeout_str) = param_as_str("timeout", &params) {
                match timeout_str.parse() {
                    // correctly parsed specified timeout
                    Ok(timeout_millis) => Some(Duration::from_millis(timeout_millis)),
                    Err(_) => return None, // failed to parse specified timeout
                }
            } else {
                None // no specified timeout
            };
            let expression = if let Some(e) = param_as_str("evaluate", &params) {
                Some(e.to_string())
            } else {
                if param_as_flag("evaluate", &params)? {
                    None
                } else {
                    return None;
                }
            };
            return Some(PostParams::Evaluate {
                expression,
                timeout,
            });
        };
        None
    }
}

/// Parse a given parameter as a boolean, where its presence without a mapping
/// is interpreted as true. If it is mapped to multiple values, or mapped to
/// something other than "true" or "false", return `None`.
fn param_as_flag<'a, 'b>(param: &'b str, params: &'a HashMap<String, Vec<String>>) -> Option<bool> {
    match params.get(param).map(Vec::as_slice) {
        Some([]) => Some(true),
        None => Some(false),
        _ => None,
    }
}

/// Parse a given parameter as a string, where its presence without a mapping
/// (or its absence entirely) is interpreted as the empty string. If it is
/// mapped to multiple values, retrun `None`.
fn param_as_str<'a, 'b>(
    param: &'b str,
    params: &'a HashMap<String, Vec<String>>,
) -> Option<&'a str> {
    match params.get(param).map(Vec::as_slice) {
        Some([string]) => Some(string.as_ref()),
        _ => None,
    }
}

fn param_as_strs<'a, 'b: 'a>(
    param: &'b str,
    params: &'a HashMap<String, Vec<String>>,
) -> Option<impl Iterator<Item = &'a str>> {
    match params.get(param) {
        Some(strings) => Some(strings.iter().map(|string| string.as_ref())),
        None => None,
    }
}

/// Parse a query string into a mapping from key to list of values. The syntax
/// expected for an individual key-value mapping is one of `k`, `k=`, `k=v`, and
/// mappings are concatenated by `&`, as in: `k1=v1&k2=v2`.
fn query_params<'a>(query: &'a str) -> Option<HashMap<String, Vec<String>>> {
    let mut map: HashMap<String, Vec<String>> = HashMap::new();
    let raw: Vec<(String, String)> = serde_urlencoded::from_str(query).unwrap();
    for (key, value) in raw {
        let key = key.trim();
        if key == "" {
            return None;
        }
        let existing = map.entry(key.to_string()).or_insert_with(Vec::new);
        if !value.is_empty() {
            existing.push(value.to_string());
        }
    }
    Some(map)
}

/// If the keys of the hashmap are exclusively within the set enumerated by the
/// slice, return `true`, otherwise return `false`.
fn constrained_to_keys<T>(map: &HashMap<String, T>, valid: &[&str]) -> bool {
    for key in map.keys() {
        let mut ok = false;
        for valid_key in valid {
            if key == *valid_key {
                ok = true;
                break;
            }
        }
        if !ok {
            return false;
        }
    }
    true
}
