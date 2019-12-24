use std::collections::HashMap;

pub(crate) struct RetrieveParams {
    pub stream: bool,
}

impl RetrieveParams {
    pub fn parse(query: &str) -> Option<RetrieveParams> {
        let params = query_map(query)?;
        if !constrained_to_keys(&params, &["stream"]) { return None }
        match params.get("stream").map(Vec::as_slice) {
            // Client wants whole page
            None | Some(["false"]) =>
                Some(RetrieveParams{stream: false}),
            // Client wants event stream of changes to page
            Some([]) | Some(["true"]) =>
                Some(RetrieveParams{stream: true}),
            // Bad client request (mapping "events" key to non-boolean)
            _ => None,
        }
    }
}

pub(crate) enum PublishParams<'a> {
    Dynamic {
        title: Option<&'a str>,
    },
    Static,
}

impl<'a> PublishParams<'a> {
    pub fn parse(query: &'a str) -> Option<PublishParams<'a>> {
        let params = query_map(query)?;
        if !constrained_to_keys(&params, &["static", "title"]) {
            return None
        }
        match params.get("static").map(Vec::as_slice) {
            Some([]) | Some(["true"]) => {
                if let Some(_) = params.get("title") { return None; }
                Some(PublishParams::Static)
            },
            None | Some(["false"]) => {
                match params.get("title").map(Vec::as_slice) {
                    None          => Some(None),
                    Some([])      => Some(Some("")),
                    Some([title]) => Some(Some(*title)),
                    Some(_)       => None,
                }.map(|title| PublishParams::Dynamic{title})
            },
            _ => None,
        }
    }
}

fn query_map<'a>(query: &'a str) -> Option<HashMap<&'a str, Vec<&'a str>>> {
    let mut map: HashMap<&'a str, Vec<&'a str>> = HashMap::new();
    if query == "" { return Some(map); }
    for mapping in query.split("&") {
        match mapping.split("=").collect::<Vec<_>>().as_mut_slice() {
            &mut [key, values] => {
                let key = key.trim();
                if key == "" { return None }
                for value in values.split(";") {
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
