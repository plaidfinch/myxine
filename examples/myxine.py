from typing import Optional, Iterator, Dict, List, Any
from copy import deepcopy
from dataclasses import *
import requests
from requests import RequestException
import json

@dataclass
class Event:
    """An event, as parsed from a text/event-stream"""
    event: str = ''
    id:    str = ''
    data:  str = ''

def parse_event_stream(lines : Iterator[str]) -> Iterator[Event]:
    """Parse an iterable of lines from the text/event-stream format, as
    defined by the W3C Recommendation on Server-Sent Events:
    https://www.w3.org/TR/eventsource/#event-stream-interpretation
    """
    event, data_lines = Event(), []
    for line in lines:
        if line == '':
            # Dispatch the event
            event.data = '\n'.join(data_lines)
            yield event
            # Reset and continue
            event, data_lines = Event(), []
        else:
            # Extract the field name and value
            try:    field, value = line.split(':', maxsplit=1)
            except: field, value = line, ''
            if value.startswith(' '): value = value[1:]

            # Set the appropriate field of the event, if any
            if   field == 'event': event.event = value
            elif field == 'id':    event.id    = value
            elif field == 'data':  data_lines.append(value)

# The default port on which myxine operates; can be overridden in the below
# functions if the server is running on another port.
MYXINE_DEFAULT_PORT = 1123

class PageEvent:
    """A PageEvent is a special case of an event from a text/event-stream,
    where the data is a dictionary from strings to values, representing the
    fields requested by the subscription.
    """
    type:   str
    target: str
    __data: str
    __mapping: Optional[Dict[str, Any]] = None
    __immutable: bool = False

    def __init__(self, wrapped : Event) -> None:
        self.type = wrapped.event
        self.target = wrapped.id
        self.__data = wrapped.data
        self.__immutable = True

    # Accessing fields looks them up in the dictionary
    def __getattr__(self, key : str) -> Any:
        if self.__mapping is None:
            # Delay parsing until first lookup, then cache results
            try: self.__mapping = json.loads(self.__data)
            except: self.__mapping = {}
        if key == 'id':
            try: return self.__mapping['currentTarget']['id']
            except: return None
        val = self.__mapping.get(key)
        if val is not None:
            return deepcopy(val)
        else:
            raise AttributeError(f"{self.__class__.__name__} object has no attribute {key}")

    # Block setting attributes
    def __setattr__(self, key : str, val : str) -> None:
        if self.__immutable and key != '_PageEvent__mapping':
            raise AttributeError(f"{self.__class__.__name__} object is immutable: {key} cannot be set")
        else:
            super().__setattr__(key, val)

def page_url(path : str, port : int = MYXINE_DEFAULT_PORT) -> str:
    """Normalize a port & path to give the localhost url for that location."""
    if len(path) > 0 and path[0] == '/': path = path[1:]
    return 'http://localhost:' + str(port) + '/' + path

def subscribe(path : str,
              subscription : Dict[str, Dict[str, List[str]]],
              port : int = MYXINE_DEFAULT_PORT) -> Iterator[PageEvent]:
    """Subscribe to a stream of page events from a myxine server, returning an
    iterator over the events returned by the stream as they become available.
    """
    url = page_url(path, port) + '?subscribe'
    try:
        response = requests.post(url, stream=True, json=subscription)
        if response.encoding is None: response.encoding = 'utf-8'
        for event in parse_event_stream(response.iter_lines(decode_unicode=True)):
            if event.data is not '': # filter out heartbeat events
                yield PageEvent(event)
    except RequestException as e:
        raise ValueError("Connection issue with myxine server (is it running?):", e)

def update(path : str,
           body : str,
           title : Optional[str] = None,
           port : int = MYXINE_DEFAULT_PORT) -> None:
    """Set the contents of the page at the given path to a provided body and
    title. If body or title is not provided, clears those elements of the page.
    """
    url = page_url(path, port)
    try: requests.post(url, data=body.encode(), params={'title': title})
    except RequestException as e:
        raise ValueError("Connection issue with myxine server (is it running?):", e)

def static(path : str,
           body : bytes,
           content_type : str,
           port : int = MYXINE_DEFAULT_PORT) -> None:
    """Set the contents of the page at the given path to the static content
    provided, as a bytestring. You must specify a content type, or else the
    browser won't necessarily know how to display this content.
    """
    url = page_url(path, port) + '?static'
    try: requests.post(url, data=body, headers={'Content-Type': content_type})
    except RequestException as e:
        raise ValueError("Connection issue with myxine server (is it running?):", e)
