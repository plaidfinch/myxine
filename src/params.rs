use std::collections::HashMap;
use percent_encoding::percent_decode;
use std::borrow::Cow;

/// Parsed parameters from a query string for a GET/HEAD request.
pub(crate) struct GetParams;

impl GetParams {
    pub fn parse(query: &str) -> Option<GetParams> {
        let params = query_params(query)?;
        if constrained_to_keys(&params, &[]) {
            Some(GetParams)
        } else {
            None
        }
    }
}

/// Parsed parameters from a query string for a POST request of a .
pub(crate) enum PostParams {
    Dynamic{title: String},
    Static,
    Subscribe,
}

impl PostParams {
    pub fn parse(query: &str) -> Option<PostParams> {
        let params = query_params(query)?;
        if let Some([]) = params.get("subscribe").map(Vec::as_slice) {
            if constrained_to_keys(&params, &["events"]) {
                Some(PostParams::Subscribe)
            } else {
                None
            }
        } else if param_as_bool("static", &params)? {
            if constrained_to_keys(&params, &["static"]) {
                Some(PostParams::Static)
            } else {
                None
            }
        } else {
            let title = param_as_str("title", &params)?.to_string();
            if constrained_to_keys(&params, &["title"]) {
                Some(PostParams::Dynamic{title})
            } else {
                None
            }
        }
    }
}

fn param_as_bool<'a, 'b: 'a>(param: &'b str, params: &'a HashMap<&'a str, Vec<Cow<'a, str>>>) -> Option<bool> {
    match params.get(param).map(Vec::as_slice) {
        Some([boolean]) => match boolean.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        Some([]) => Some(true),
        None => Some(false),
        _ => None,
    }
}

fn param_as_str<'a, 'b: 'a>(param: &'b str, params: &'a HashMap<&'a str, Vec<Cow<'a, str>>>) -> Option<&'a str> {
    match params.get(param).map(Vec::as_slice) {
        Some([string]) => Some(string.as_ref()),
        Some([]) | None => Some(""),
        _ => None,
    }
}


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
