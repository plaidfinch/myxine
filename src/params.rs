use std::collections::HashMap;
use percent_encoding::percent_decode;
use std::borrow::Cow;

/// Parsed parameters from a query string for a GET/HEAD request.
pub(crate) enum GetParams {
    FullPage,
    PageUpdates,
}

impl GetParams {
    /// Parse a query string from a GET request.
    pub fn parse(query: &str) -> Option<GetParams> {
        let params = query_params(query)?;
        if param_as_bool("updates", &params)?
        && constrained_to_keys(&params, &["updates"]) {
            Some(GetParams::PageUpdates)
        } else if constrained_to_keys(&params, &[]) {
            Some(GetParams::FullPage)
        } else {
            None
        }
    }
}

/// Parsed parameters from a query string for a POST request.
pub(crate) enum PostParams {
    DynamicPage{title: String},
    StaticPage,
    SubscribeEvents,
    PageEvent{event: String, id: String},
}

impl PostParams {
    /// Parse a query string from a POST request.
    pub fn parse(query: &str) -> Option<PostParams> {
        let params = query_params(query)?;
        if param_as_bool("subscribe", &params)?
            && constrained_to_keys(&params, &["subscribe"]) {
                return Some(PostParams::SubscribeEvents)
        } else if param_as_bool("static", &params)?
            && constrained_to_keys(&params, &["static"]) {
                return Some(PostParams::StaticPage)
        } else if let (Some(event), Some(id)) =
            (param_as_str("event", &params)?.map(String::from),
             param_as_str("id", &params)?.map(String::from)) {
                if constrained_to_keys(&params, &["event", "id"])
                && event != "" && id != "" {
                    return Some(PostParams::PageEvent{event, id})
                }
        } else if constrained_to_keys(&params, &["title"]) {
            let title = param_as_str("title", &params)?.unwrap_or("").to_string();
            return Some(PostParams::DynamicPage{title})
        }
        return None;
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
fn param_as_str<'a, 'b: 'a>(param: &'b str, params: &'a HashMap<&'a str, Vec<Cow<'a, str>>>) -> Option<Option<&'a str>> {
    match params.get(param).map(Vec::as_slice) {
        Some([string]) => Some(Some(string.as_ref())),
        Some([]) => Some(Some("")),
        None => Some(None),
        _ => None,
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
                    map.entry(key).or_insert_with(|| vec![])
                        .push(percent_decode(value.as_bytes()).decode_utf8_lossy());
                }
            },
            [key] => {
                let key = key.trim();
                map.entry(key).or_insert_with(|| vec![]);
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
