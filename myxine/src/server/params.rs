use std::collections::{HashMap, HashSet};
use std::time::Duration;
use percent_encoding::percent_decode;
use std::borrow::Cow;
use uuid::Uuid;
use crate::page::Subscription;

/// Parsed parameters from a query string for a GET/HEAD request.
pub(crate) enum GetParams {
    FullPage,
    PageUpdates,
    Subscribe(Subscription)
}

impl GetParams {
    /// Parse a query string from a GET request.
    pub fn parse(query: &str) -> Option<GetParams> {
        let params = query_params(query)?;
        if constrained_to_keys(&params, &[]) {
            Some(GetParams::FullPage)
        } else if param_as_bool("updates", &params)?
            && constrained_to_keys(&params, &["updates"])
        {
            Some(GetParams::PageUpdates)
        } else if constrained_to_keys(&params, &["events", "event"]) {
            Some(GetParams::Subscribe(parse_subscription(&params)))
        } else {
            None
        }
    }
}

fn parse_subscription<'a>(params: &'a HashMap<&'a str, Vec<Cow<'a, str>>>) -> Subscription {
    let events = match (param_as_strs("events", &params),
           param_as_strs("event", &params)) {
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
    DynamicPage{title: String},
    StaticPage,
    Evaluate{expression: Option<String>, timeout: Option<Duration>},
    ChangeSubscription{id: Uuid, subscription: Subscription},
    // TODO: Add validation key to page-sent events to prevent MITM?
    PageEvent,
    EvalResult{id: Uuid},
    EvalError{id: Uuid},
}

impl PostParams {
    /// Parse a query string from a POST request.
    pub fn parse(query: &str) -> Option<PostParams> {
        let params = query_params(query)?;
        if constrained_to_keys(&params, &[]) {
            return Some(PostParams::DynamicPage{title: "".to_string()})
        } else if constrained_to_keys(&params, &["title"]) {
            let title = param_as_str("title", &params)?.to_string();
            return Some(PostParams::DynamicPage{title})
        } else if constrained_to_keys(&params, &["static"]) {
            if param_as_bool("static", &params)? {
                return Some(PostParams::StaticPage)
            }
        } else if let Some(id) = param_as_str("subscription", &params).and_then(|s| Uuid::parse_str(s).ok()) {
            let subscription = parse_subscription(&params);
            return Some(PostParams::ChangeSubscription{id, subscription})
        } else if constrained_to_keys(&params, &["result"]) {
            let id = Uuid::parse_str(param_as_str("result", &params)?).ok()?;
            return Some(PostParams::EvalResult{id})
        } else if constrained_to_keys(&params, &["error"]) {
            let id = Uuid::parse_str(param_as_str("error", &params)?).ok()?;
            return Some(PostParams::EvalError{id})
        } else if constrained_to_keys(&params, &["event"]) {
            if param_as_bool("event", &params)? {
                return Some(PostParams::PageEvent)
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
            let expression = if let Some(true) = param_as_bool("evaluate", &params) {
                None
            } else {
                Some(param_as_str("evaluate", &params)?.to_string())
            };
            return Some(PostParams::Evaluate{expression, timeout})
        };
        None
    }
}

/// Parse a given parameter as a boolean, where its presence without a mapping
/// is interpreted as true. If it is mapped to multiple values, or mapped to
/// something other than "true" or "false", return `None`.
fn param_as_bool<'a, 'b: 'a>(param: &'b str, params: &'a HashMap<&'a str, Vec<Cow<'a, str>>>) -> Option<bool> {
    match params.get(param).map(Vec::as_slice) {
        Some([]) => Some(true),
        None => Some(false),
        _ => None,
    }
}

/// Parse a given parameter as a string, where its presence without a mapping
/// (or its absence entirely) is interpreted as the empty string. If it is
/// mapped to multiple values, retrun `None`.
fn param_as_str<'a, 'b: 'a>(param: &'b str, params: &'a HashMap<&'a str, Vec<Cow<'a, str>>>) -> Option<&'a str> {
    match params.get(param).map(Vec::as_slice) {
        Some([string]) => Some(string.as_ref()),
        _ => None,
    }
}

fn param_as_strs<'a, 'b: 'a>(
    param: &'b str,
    params: &'a HashMap<&'a str, Vec<Cow<'a, str>>>
) -> Option<impl Iterator<Item = &'a str>> {
    match params.get(param) {
        Some(strings) => Some(strings.into_iter().map(|string| string.as_ref())),
        None => None,
    }
}

/// Parse a query string into a mapping from key to list of values. The syntax
/// expected for an individual key-value mapping is one of `k`, `k=`, `k=v`,
/// `k=v1,v2`, etc., and mappings are concatenated by `&`, as in:
/// `k1=v1,v2&k2=v3,v4`. Values are URL-percent-decoded prior to being returned,
/// whereas keys are required to be URL-safe strings.
fn query_params<'a>(query: &'a str) -> Option<HashMap<&'a str, Vec<Cow<'a, str>>>> {
    let mut map: HashMap<&'a str, Vec<Cow<'a, str>>> = HashMap::new();
    if query == "" { return Some(map); }
    for mapping in query.split('&') {
        match mapping.split('=').collect::<Vec<_>>().as_mut_slice() {
            [key, values] => {
                let key = key.trim();
                if key == "" { return None }
                for value in values.split(',') {
                    let value = value.trim();
                    map.entry(key).or_insert_with(Vec::new)
                        .push(percent_decode(value.as_bytes()).decode_utf8_lossy());
                }
            },
            [key] => {
                let key = key.trim();
                map.entry(key).or_insert_with(Vec::new);
            }
            _ => return None,
        }
    }
    Some(map)
}

/// If the keys of the hashmap are exclusively within the set enumerated by the
/// slice, return `true`, otherwise return `false`.
fn constrained_to_keys<'a, K: PartialEq + 'a, V>(
    map: &HashMap<K, V>,
    valid: &[K]
) -> bool {
    for key in map.keys() {
        let mut ok = false;
        for valid_key in valid {
            if key == valid_key {
                ok = true;
                break;
            }
        }
        if !ok { return false; }
    }
    true
}
