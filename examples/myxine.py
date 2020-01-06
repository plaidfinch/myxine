from typing import Optional, Iterator, Dict, List, Any
from dataclasses import *
import requests
from requests import RequestException
import json

@dataclass
class Event:
    """An event, as parsed from a text/event-stream"""
    event: Optional[str] = None
    id:    Optional[str] = None
    data:  Optional[str] = None
    retry: Optional[int] = None

def parse_event_stream(lines : Iterator[str]) -> Iterator[Event]:
    """Parse an iterable of lines from a stream conforming to the
    text/event-stream format, as defined in the HTML living standard section
    on server-sent events:
    https://html.spec.whatwg.org/multipage/server-sent-events.html#parsing-an-event-stream"""
    within_event = False
    event = Event()
    for line in lines:
        if line == '' and within_event:
            # Dispatch the event when we hit an empty line
            yield event
            # Reset and loop
            within_event = False
            event = Event()
        elif line.startswith(':'):
            # This is a comment line
            pass
        else:
            # We are now processing an event
            within_event = True

            # Pre-process the line to extract the field name and value
            try:
                field, value = line.split(':', maxsplit=1)
            except ValueError:
                field = line
                value = ''
            if len(value) > 0 and value[0] == ' ':
                value = value[1:]

            # Dispatch on field name to set fields of the Event
            if field == 'event':
                event.event = value
            elif field == 'id' and '\0' not in value:
                event.id = value
            elif field == 'data':
                if event.data is None:
                    event.data = value
                else:
                    event.data += value + '\n'
            elif field == 'retry':
                try:
                    event.retry = int(value)
                except ValueError: pass

# The default port on which myxine operates; can be overridden in the below
# functions if the server is running on another port.
MYXINE_DEFAULT_PORT = 1123

class PageEvent:
    """A PageEvent is a special case of an event from a text/event-stream,
    where the data is a dictionary from strings to values, representing the
    fields requested by the subscription.
    """
    __event : str
    __id : str
    __mapping : Dict[str, Any]

    def __init__(self, wrapped : Event) -> None:
        if wrapped.event is not None:
            self.__event = wrapped.event
        else:
            self.__event = ''
        if wrapped.id is not None:
            self.__id = wrapped.id
        else:
            self.__id = ''
        if wrapped.data is not None:
            self.mapping = json.loads(wrapped.data)
        else:
            self.mapping = {}

    def __getitem__(self, key : str) -> Optional[Any]:
        return self.mapping.get(key)

    def event(self) -> str:
        """Get the event type for this page event."""
        return self.__event

    def id(self) -> str:
        """Get the target id for this page event."""
        return self.__id

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
    try:
        url = page_url(path, port) + '?subscribe'
        r = requests.post(url, stream=True, json=subscription)
        if r.encoding is None: r.encoding = 'utf-8'
        for event in parse_event_stream(r.iter_lines(decode_unicode=True)):
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
    try:
        url = page_url(path, port)
        params : Dict[str, str]
        if title is None: params = {}
        else: params = {'title': title}
        r = requests.post(url, data=body.encode(), params=params)
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
    try:
        url = page_url(path, port) + '?static'
        requests.post(url, data=body, headers={'Content-Type': content_type})
    except RequestException as e:
        raise ValueError("Connection issue with myxine server (is it running?):", e)
