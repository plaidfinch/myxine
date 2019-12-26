use std::collections::HashMap;
use percent_encoding::percent_decode;

/// Parsed parameters from a query string for a GET/HEAD request.
pub(crate) struct RetrieveParams { }

impl RetrieveParams {
    pub fn parse(query: &str) -> Option<RetrieveParams> {
        let params = query_map(query)?;
        if constrained_to_keys(&params, &[]) {
            Some(RetrieveParams{})
        } else {
            None
        }
    }
}

/// Parsed parameters from a query string for a POST request of a .
pub(crate) struct PublishParams {
    pub title: Option<String>,
}

impl<'a> PublishParams {
    pub fn parse(query: &str) -> Option<PublishParams> {
        let decoded = percent_decode(query.as_bytes()).decode_utf8_lossy();
        Some(PublishParams {
            title: if query == "" { None } else { Some(decoded.to_string()) }
        })
    }
}

// fn query_map_tokenizing<'a>(query: &'a str) -> Option<HashMap<&'a str, Vec<&'a str>>> {
//     let mut map: HashMap<&'a str, Vec<&'a str>> = HashMap::new();
//     let mut key = None;
//     let mut val = None;
//     let mut quoted = false;
//     let mut escaping = false;
//     for (i, c) in query.chars().enumerate() {
//         match c {
//             '"' => if !escaping {
//                 quoted = !quoted
//             } else {
//                 escaping =
//             },
//             '\\' if !escaping => escaping = true,
//         }
//     }
//     return Some(map);
// }

// TODO: Do a better job of parsing the query string

fn query_map<'a>(query: &'a str) -> Option<HashMap<&'a str, Vec<&'a str>>> {
    let mut map: HashMap<&'a str, Vec<&'a str>> = HashMap::new();
    if query == "" { return Some(map); }
    for mapping in query.split("&") {
        match mapping.split("=").collect::<Vec<_>>().as_mut_slice() {
            &mut [key, values] => {
                let key = key.trim();
                if key == "" { return None }
                for value in values.split(",") {
                    let value = value.trim();
                    map.entry(key).or_insert(vec![]).push(value);
                }
            },
            &mut [key] => {
                let key = key.trim();
                map.entry(key).or_insert(vec![]);
            }
            _ => return None,
        }
    }
    return Some(map);
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
    return true;
}
