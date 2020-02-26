from typing import Optional, Iterator, Dict, List, Any
from copy import deepcopy
from dataclasses import dataclass
import requests
from requests import RequestException
import json


@dataclass
class Event:
    """An event, as parsed from a text/event-stream"""
    event_type: str = ''
    event_id: str = ''
    event_data:  str = ''


def parse_event_stream(lines: Iterator[str]) -> Iterator[Event]:
    """Parse an iterable of lines from the text/event-stream format, as
    defined by the W3C Recommendation on Server-Sent Events:
    https://www.w3.org/TR/eventsource/#event-stream-interpretation
    """
    data_lines: List[str] = []
    event = Event()
    for line in lines:
        if line == '':
            # Dispatch the event
            event.event_data = '\n'.join(data_lines)
            yield event
            # Reset and continue
            event, data_lines = Event(), []
        else:
            # Extract the field name and value
            try:
                field, value = line.split(':', maxsplit=1)
            except ValueError:
                field, value = line, ''
            if value.startswith(' '):
                value = value[1:]

            # Set the appropriate field of the event, if any
            if field == 'event':
                event.event_type = value
            elif field == 'id':
                event.event_id = value
            elif field == 'data':
                data_lines.append(value)


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

    def __init__(self, wrapped: Event) -> None:
        self.type = wrapped.event_type
        self.target = wrapped.event_id
        self.__data = wrapped.event_data
        self.__immutable = True

    # Accessing fields looks them up in the dictionary
    def __getattr__(self, key: str) -> Any:
        if self.__mapping is None:
            # Delay parsing until first lookup, then cache results
            try:
                self.__mapping = json.loads(self.__data)
            except json.JSONDecodeError:
                self.__mapping = {}
        val = self.__mapping.get(key)
        if val is not None:
            return deepcopy(val)
        else:
            msg = f"{self.__class__.__name__} object has no attribute {key}"
            raise AttributeError(msg)

    # Block setting attributes
    def __setattr__(self, key: str, val: str) -> None:
        if self.__immutable and key != '_PageEvent__mapping':
            msg = f"{self.__class__.__name__} is immutable: {key} can't be set"
            raise AttributeError(msg)
        else:
            super().__setattr__(key, val)


def page_url(path: str, port: int = MYXINE_DEFAULT_PORT) -> str:
    """Normalize a port & path to give the localhost url for that location."""
    if len(path) > 0 and path[0] == '/':
        path = path[1:]
    return 'http://localhost:' + str(port) + '/' + path


def events(path: str,
           subscription: Optional[List[str]] = None,
           port: int = MYXINE_DEFAULT_PORT) -> Iterator[PageEvent]:
    """Subscribe to a stream of page events from a myxine server, returning an
    iterator over the events returned by the stream as they become available.
    """
    url = page_url(path, port)
    try:
        params: Dict[str, List[str]]
        if subscription is None:
            url = url + "?events"
            params = {}
        else:
            params = {'events': subscription}
        response = requests.get(url, stream=True, params=params)
        if response.encoding is None:
            response.encoding = 'utf-8'
        stream = parse_event_stream(response.iter_lines(decode_unicode=True))
        for event in stream:
            if event.event_data != '':  # filter out heartbeat events
                yield PageEvent(event)
    except RequestException as e:
        msg = "Connection issue with myxine server (is it running?)"
        raise ValueError(msg, e)


def evaluate(path: str, *,
             expression: Optional[str] = None,
             statement: Optional[str] = None,
             timeout: Optional[int] = None,
             port: int = MYXINE_DEFAULT_PORT) -> None:
    """Evaluate the given JavaScript code in the context of the page."""
    bad_args_err = \
        ValueError('Input must be exactly one of a statement or an expression')
    if expression is not None:
        if statement is not None:
            raise bad_args_err
        url = page_url(path, port)
        params = {'evaluate': expression}
        data = expression.encode()
    elif statement is not None:
        if expression is not None:
            raise bad_args_err
        url = page_url(path, port) + '?evaluate'
        params = {}
        data = statement.encode()
    else:
        raise bad_args_err
    if timeout is not None:
        params['timeout'] = str(timeout)
    try:
        r = requests.post(url, data=data, params=params)
        if r.status_code == 200:
            return r.json()
        else:
            raise ValueError(r.text)
    except RequestException as e:
        msg = "Connection issue with myxine server (is it running?)"
        raise ValueError(msg, e)


def update(path: str,
           body: str,
           title: Optional[str] = None,
           port: int = MYXINE_DEFAULT_PORT) -> None:
    """Set the contents of the page at the given path to a provided body and
    title. If body or title is not provided, clears those elements of the page.
    """
    url = page_url(path, port)
    try:
        requests.post(url, data=body.encode(), params={'title': title})
    except RequestException as e:
        msg = "Connection issue with myxine server (is it running?)"
        raise ValueError(msg, e)


def static(path: str,
           body: bytes,
           content_type: str,
           port: int = MYXINE_DEFAULT_PORT) -> None:
    """Set the contents of the page at the given path to the static content
    provided, as a bytestring. You must specify a content type, or else the
    browser won't necessarily know how to display this content.
    """
    url = page_url(path, port) + '?static'
    try:
        requests.post(url, data=body, headers={'Content-Type': content_type})
    except RequestException as e:
        msg = "Connection issue with myxine server (is it running?)"
        raise ValueError(msg, e)
