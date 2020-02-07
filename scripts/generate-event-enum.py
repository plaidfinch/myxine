#! /usr/bin/env python3

import sys
import json
from typing import *

def generate_rust(events):
    # Generate names of variants
    variants = []
    for name_words, properties in events:
        name = ''.join(word.title() for word in name_words)
        variants.append((name, properties))
    # Actually output the text
    lines = []
    lines.append('#[non_exhaustive]')
    lines.append('#[derive(Clone, Debug, Serialize, Deserialize)]')
    lines.append('#[serde(rename_all = "lowercase", tag = "event", content = "properties")]')
    lines.append('enum Event {')
    for name, properties in variants:
        lines.append('    #[non_exhaustive]')
        lines.append('    ' + name + ' { ')
        items = list(properties.items())
        items.sort()
        for field, type in items:
            lines.append('        ' + field + ': ' + type + ',')
        lines.append('    },')
    lines.append('}')
    return '\n'.join(lines)

languages = {
    'rust': generate_rust
}

def main():
    try: _, language, filename = sys.argv
    except: print("Wrong number of arguments: please specify output language and interface definition JSON file.", file=sys.stderr)

    try: generate = languages[language]
    except: print("Invalid language: " + language, file=sys.stderr)

    try:
        with open(filename) as x: spec = json.loads(x.read())
    except: print("Couldn't open file: " + filename, file=sys.stderr)
    spec_events = spec['events']
    spec_interfaces = spec['interfaces']
    events = []
    for event, event_info in spec_events.items():
        interface = event_info['interface']
        name_words = event_info['nameWords']
        fields = accum_fields(interface, spec_interfaces)
        events.append((name_words, fields))
    print(generate(events))

# Accumulate all the fields in all super-interfaces of the given interface
def accum_fields(interface, interfaces):
    properties = {}
    while True:
        for property, type in interfaces[interface]['properties'].items():
            if properties.get(property) is None:
                properties[property] = type
        if interfaces[interface]['inherits'] is None: break
        else: interface = interfaces[interface]['inherits']
    return properties

if __name__ == '__main__': main()
