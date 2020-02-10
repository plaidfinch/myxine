#! /usr/bin/env python3

import sys
import json
from typing import *

graph_parameters = [
    'rankdir="LR";',
    'splines=false;',
    'node [shape=none, fontname="Courier", fontsize="18pt", overlap=false, penwidth=2];',
    'edge [arrowtail=none, dir=both, overlap=false, concentrate=true, penwidth=2];',
    'outputorder="edgesfirst";',
    'target="_blank"',
]

interface_parameters = [
    'node [fontcolor="blue", fontsize="20pt", margin="-1"];'
]

event_parameters = [
    'node [shape=ellipse, fontcolor="blue", style="filled"];'
]

def generate_node(name, attributes, *, quotes=True):
    attr_list = []
    if quotes: quote = '"'
    else: quote = ''
    for attr, value in attributes.items():
        attr_list.append(attr + '=' + quote + value + quote)
    return '"' + name + '" [' + ', '.join(attr_list) + '];'

def generate_edge(start, end, *, start_port=None, end_port=None):
    if start_port is not None: start_port = ':' + start_port
    else: start_port = ''
    if end_port is not None: end_port = ':' + end_port
    else: end_port = ''
    return '"' + start + '"' + start_port + ' -> "' + end + '"' + end_port + ';'

def get_interface_levels(events, interfaces):
    heights = {}
    for interface in interfaces.keys():
        heights[interface] = 1
    for event, event_info in events.items():
        interface = event_info['interface']
        height = 1
        while True:
            heights[interface] = max(heights[interface], height)
            height += 1
            interface = interfaces[interface].get('inherits')
            if interface is None: break
    levels = {}
    for interface, height in heights.items():
        if levels.get(height) is None:
            levels[height] = []
        levels[height].append(interface)
    return levels

def generate(events, interfaces):
    # Figure out the groupings of interfaces by height
    interface_levels = get_interface_levels(events, interfaces)

    # Sort the events by interface and then by name
    events = sorted(events.items(), key=lambda e: (e[1]['interface'], e[0]))

    lines = ['digraph {']
    lines.extend('  ' + s for s in graph_parameters)

    # The interface nodes
    for _, same_rank_interfaces in sorted(interface_levels.items(), reverse=True):
        lines.append('  {')
        lines.append('    rank=same;')
        lines.extend('    ' + s for s in interface_parameters)
        for interface in same_rank_interfaces:
            interface_info = interfaces[interface]
            docs = "https://developer.mozilla.org/docs/Web/API/" + interface
            properties = ''.join('<TR>' + \
                                 f'<TD HREF="{docs + "/" + property}" BGCOLOR="white" ALIGN="LEFT">' + \
                                 property + '<FONT COLOR="black">:</FONT></TD>' + \
                                 '<TD BGCOLOR="white" ALIGN="LEFT"><FONT COLOR="black">' + \
                                 type + '</FONT></TD>' + \
                                 '</TR>' \
                                 for property, type in interface_info['properties'].items())
            label = f'<<TABLE BORDER="1" CELLPADDING="4" CELLSPACING="0">' \
                  + f'<TR><TD COLSPAN="2" BGCOLOR="lightblue" PORT="interface" HREF="{docs}">{interface}</TD></TR>' \
                  + properties \
                  + '</TABLE>>'
            attributes = {'label': label}
            lines.append('    ' + generate_node(interface, attributes, quotes=False))
        lines.append('  }')

    # The event nodes
    lines.append('  {')
    lines.extend('    ' + s for s in event_parameters)
    lines.append('    rank=same;')
    for event, event_info in events:
        if event_info['bubbles']:
            attrs = {'fillcolor': 'lightgreen'}
        else:
            attrs = {'fillcolor': 'yellow'}
        attrs['href'] = "https://developer.mozilla.org/docs/Web/Events/" + event
        lines.append('    ' + generate_node(event, attrs))
    lines.append('  }')

    # The interface inheritance edges
    for interface, interface_info in interfaces.items():
        inherits = interface_info['inherits']
        if inherits is not None:
            lines.append('  ' + generate_edge(inherits, interface,
                                              start_port='interface:e',
                                              end_port='interface:w'))

    # The event inheritance edges
    for event, event_info in events:
        interface = event_info['interface']
        if inherits is not None:
            lines.append('  ' + generate_edge(interface, event, start_port='interface:e'))

    lines.append('}')
    return '\n'.join(lines)

def main():
    try: _, filename = sys.argv
    except: print("Wrong number of arguments: please specify interface definition file.",
                  file=sys.stderr)

    try:
        with open(filename) as x: spec = json.loads(x.read())
    except: print("Couldn't open file: " + filename, file=sys.stderr)
    spec_events = spec['events']
    spec_interfaces = spec['interfaces']
    print(generate(spec_events, spec_interfaces))

if __name__ == '__main__': main()
