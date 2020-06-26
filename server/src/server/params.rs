use serde_urlencoded;
use std::collections::{HashMap, HashSet};
use std::time::Duration;
use std::fmt::{Display, Formatter};

use myxine::page::Subscription;
use myxine::unique::Unique;
use myxine::page::RefreshMode;

#[derive(Debug, Clone)]
pub enum ParseError {
    ExpectedFlag(&'static str),
    ExpectedOne(&'static str),
    Unexpected(HashMap<String, Vec<String>>, Vec<String>),
    Custom(&'static str, String, &'static str),
}

impl warp::reject::Reject for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            ParseError::ExpectedFlag(flag) =>
                write!(f, "Expected query parameter '{}' as a flag with no value", flag),
            ParseError::ExpectedOne(param) =>
                write!(f, "Expected query parameter '{}' with exactly one argument", param),
            ParseError::Unexpected(map, valid) => {
                write!(f, "Unexpected query parameter{}: ",
                       if map.len() > 1 { "s" } else { "" })?;
                let mut i = 1;
                // write!(f, "?")?;
                for (key, vals) in map {
                    // let mut j = 1;
                    // for val in vals {
                    //     if val != "" {
                    //         write!(f, "{}={}", key, val)?;
                    //     } else {
                    write!(f, "'{}'", key)?;
                    //     }
                    //     if j < vals.len() {
                    //         write!(f, "&")?;
                    //     }
                    //     j += 1;
                    // }
                    // if i < map.len() {
                    //     write!(f, "&")?;
                    // }
                    if i < map.len() {
                        write!(f, ", ")?;
                    }
                    i += 1;
                }
                write!(f, "\nExpecting only: ")?;
                i = 1;
                for param in valid {
                    write!(f, "'{}'", param)?;
                    if i < valid.len() {
                        write!(f, ", ")?;
                    }
                    i += 1;
                }
                Ok(())
            },
            ParseError::Custom(param, value, expected) => {
                write!(f, "Parse error in the value of query parameter '{}={}'\nExpecting: {}", param, value, expected)
            }
        }
    }
}

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
    pub fn parse(query: &str) -> Result<GetParams, ParseError> {
        let params = query_params(&query);
        if params.is_empty() {
            Ok(GetParams::FullPage)
        } else if param_as_flag("updates", &params)? {
            constrain_to_keys(params, &["updates"])?;
            Ok(GetParams::PageUpdates)
        } else if param_as_flag("stream", &params)? {
            let result = Ok(GetParams::Subscribe {
                subscription: parse_subscription(&params),
                stream_or_after: SubscribeParams::Stream,
            });
            constrain_to_keys(params, &["events", "event", "stream"])?;
            result
        } else if let Ok(after_str) = param_as_str("after", &params) {
            let after = u64::from_str_radix(after_str, 10)
                .map_err(|_| ParseError::Custom("after", after_str.to_string(), "non-negative whole number"))
                .and_then(|n| Ok(n + if param_as_flag("next", &params)? {
                    1
                } else {
                    0
                }))?;
            let result = Ok(GetParams::Subscribe {
                subscription: parse_subscription(&params),
                stream_or_after: SubscribeParams::After(after),
            });
            constrain_to_keys(params, &["events", "event", "next", "after"])?;
            result
        } else {
            let result = Ok(GetParams::Subscribe {
                subscription: parse_subscription(&params),
                stream_or_after: SubscribeParams::Next,
            });
            constrain_to_keys(params, &["events", "event", "next"])?;
            result
        }
    }
}

fn parse_subscription<'a>(params: &'a HashMap<String, Vec<String>>) -> Subscription {
    let events = match (
        params.get("events"),
        params.get("event"),
    ) {
        (Some(e1), Some(e2)) => e1.iter().chain(e2).map(String::from).collect(),
        (Some(e1), None) => e1.iter().map(String::from).collect(),
        (None, Some(e2)) => e2.iter().map(String::from).collect(),
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
    pub fn parse(query: &str) -> Result<PostParams, ParseError> {
        let params = query_params(query);
        if let Ok(id_str) = param_as_str("page-result", &params) {
            let id = Unique::parse_str(id_str)
                .map_or_else(
                    || Err(ParseError::Custom("page-result", id_str.to_string(), "UUID")),
                    Ok
                )?;
            constrain_to_keys(params, &["page-result"])?;
            Ok(PostParams::EvalResult { id })
        } else if let Ok(id_str) = param_as_str("page-error", &params) {
            let id = Unique::parse_str(id_str)
                .map_or_else(
                    || Err(ParseError::Custom("page-error", id_str.to_string(), "UUID")),
                    Ok
                )?;
            constrain_to_keys(params, &["page-error"])?;
            Ok(PostParams::EvalError { id })
        } else if param_as_flag("page-event", &params)? {
            constrain_to_keys(params, &["page-event"])?;
            Ok(PostParams::PageEvent)
        } else if params.contains_key("expression") {
            let expression =
                param_as_str("evaluate", &params)
                .map_or_else(|err| if param_as_flag("evaluate", &params)? { Ok(None) } else { Err(err) },
                             |expression| Ok(Some(expression.to_string())))?;
            let timeout = if params.contains_key("timeout") {
                let timeout_str = param_as_str("timeout", &params)?;
                timeout_str
                    .parse()
                    .map_or_else(|_| Err(ParseError::Custom("timeout",
                                                            timeout_str.to_string(),
                                                            "non-negative number of milliseconds")),
                                 |millis| Ok(Some(Duration::from_millis(millis))))?
            } else {
                None
            };
            constrain_to_keys(params, &["evaluate", "timeout"])?;
            Ok(PostParams::Evaluate { expression, timeout })
        } else if param_as_flag("static", &params)? {
            constrain_to_keys(params, &["static"])?;
            Ok(PostParams::StaticPage)
        } else {
            let title = param_as_str("title", &params).unwrap_or("").to_string();
            let refresh = match param_as_flag("refresh", &params) {
                Ok(true) => RefreshMode::FullReload,
                Ok(false) => RefreshMode::Diff,
                Err(_) => match param_as_str("refresh", &params)? {
                    "full" => RefreshMode::FullReload,
                    "set" => RefreshMode::SetBody,
                    "diff" => RefreshMode::Diff,
                    s => Err(ParseError::Custom("refresh", s.to_string(), "one of 'full', 'set', or 'diff'"))?,
                },
            };
            constrain_to_keys(params, &["title", "refresh"])?;
            Ok(PostParams::DynamicPage { title, refresh })
        }
    }
}

/// Parse a given parameter as a boolean, where its presence without a mapping
/// is interpreted as true. If it is mapped to multiple values, or mapped to
/// something other than "true" or "false", return `None`.
fn param_as_flag<'a>(param: &'static str, params: &'a HashMap<String, Vec<String>>) -> Result<bool, ParseError> {
    match params.get(param).map(Vec::as_slice) {
        Some([]) => Ok(true),
        Some([s]) if s == "" => Ok(true),
        None => Ok(false),
        _ => Err(ParseError::ExpectedFlag(param)),
    }
}

/// Parse a given parameter as a string, where its presence without a mapping
/// (or its absence entirely) is interpreted as the empty string. If it is
/// mapped to multiple values, retrun `None`.
fn param_as_str<'a>(
    param: &'static str,
    params: &'a HashMap<String, Vec<String>>,
) -> Result<&'a str, ParseError> {
    match params.get(param).map(Vec::as_slice) {
        Some([string]) => Ok(string.as_ref()),
        _ => Err(ParseError::ExpectedOne(param)),
    }
}

/// Parse a query string into a mapping from key to list of values. The syntax
/// expected for an individual key-value mapping is one of `k`, `k=`, `k=v`, and
/// mappings are concatenated by `&`, as in: `k1=v1&k2=v2`.
pub(crate) fn query_params<'a>(query: &'a str) -> HashMap<String, Vec<String>> {
    let mut map: HashMap<String, Vec<String>> = HashMap::new();
    let raw: Vec<(String, String)> = serde_urlencoded::from_str(query).unwrap();
    for (key, value) in raw {
        let key = key.trim();
        let existing = map.entry(key.to_string()).or_insert_with(Vec::new);
        existing.push(value.to_string());
    }
    map
}

/// If the keys of the hashmap are exclusively within the set enumerated by the
/// slice, return `true`, otherwise return `false`.
pub(crate) fn constrain_to_keys(mut map: HashMap<String, Vec<String>>, valid: &[&str]) -> Result<(), ParseError> {
    for key in valid {
        map.remove(*key);
    }
    if map.is_empty() {
        Ok(())
    } else {
        Err(ParseError::Unexpected(map, valid.iter().map(|s| s.to_string()).collect()))
    }
}
