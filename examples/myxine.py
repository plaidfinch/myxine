from typing import *
from dataclasses import *
import requests
from requests import RequestException
import json
from queue import SimpleQueue
import threading
from threading import Thread, Lock

@dataclass
class Event:
    """An event, as parsed from a text/event-stream"""
    event: Optional[str] = None
    id:    Optional[str] = None
    data:  Optional[str] = None
    retry: Optional[int] = None

def parse_event_stream(lines: Iterator[str]) -> Iterator[Event]:
    """Parse an iterable of lines from a stream conforming to the
    text/event-stream format, as defined in the HTML living standard section
    on server-sent events:
    https://html.spec.whatwg.org/multipage/server-sent-events.html#parsing-an-event-stream"""
    event = Event()
    for line in lines:
        if line == '':
            # Dispatch the event when we hit an empty line
            yield event
            # Reset and loop
            event = Event()
        else:
            # Pre-process the line to extract the field name and value and trim
            # the left-most space (if any) from the value
            try: field, value = line.split(':', maxsplit=1)
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
                else: event.data += value + '\n'
            elif field == 'retry':
                try: event.retry = int(value)
                except ValueError: pass

# The default port on which myxine operates; can be overridden in the below
# functions if the server is running on another port.
MYXINE_DEFAULT_PORT = 1123

class PageEvent:
    """A PageEvent is a special case of an event from a text/event-stream,
    where the data is a dictionary from strings to values, representing the
    fields requested by the subscription.
    """
    __event: str
    __target: str
    __mapping: Dict[str, Any]

    def __init__(self, wrapped: Event) -> None:
        if wrapped.event is None: self.__event = ''
        else: self.__event = wrapped.event
        if wrapped.id is None: self.__target = ''
        else: self.__target = wrapped.id
        if wrapped.data is None: self.__mapping = {}
        else: self.mapping = json.loads(wrapped.data)

    def __getitem__(self, key: str) -> Optional[Any]:
        return self.mapping.get(key)

    def event(self) -> str:
        """Get the event type for this page event."""
        return self.__event

    def target(self) -> str:
        """Get the target id for this page event."""
        return self.__target

def page_url(path: str, port: int = MYXINE_DEFAULT_PORT) -> str:
    """Normalize a port & path to give the localhost url for that location."""
    if len(path) > 0 and path[0] == '/': path = path[1:]
    return 'http://localhost:' + str(port) + '/' + path

def subscribe(path: str,
              subscription: Dict[str, Dict[str, List[str]]],
              port: int = MYXINE_DEFAULT_PORT) -> Iterator[PageEvent]:
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

def update(path: str,
           body: str,
           title: Optional[str] = None,
           port: int = MYXINE_DEFAULT_PORT) -> None:
    """Set the contents of the page at the given path to a provided body and
    title. If body or title is not provided, clears those elements of the page.
    """
    try:
        url = page_url(path, port)
        params: Dict[str, str]
        if title is None: params = {}
        else: params = {'title': title}
        r = requests.post(url, data=body.encode(), params=params)
    except RequestException as e:
        raise ValueError("Connection issue with myxine server (is it running?):", e)

def static(path: str,
           body: bytes,
           content_type: str,
           port: int = MYXINE_DEFAULT_PORT) -> None:
    """Set the contents of the page at the given path to the static content
    provided, as a bytestring. You must specify a content type, or else the
    browser won't necessarily know how to display this content.
    """
    try:
        url = page_url(path, port) + '?static'
        requests.post(url, data=body, headers={'Content-Type': content_type})
    except RequestException as e:
        raise ValueError("Connection issue with myxine server (is it running?):", e)

# All this is fancy stuff intended to make it easy to write an event-based
# page. It's implemented in terms of the things above, and you don't need it to
# do straightforward things.

S = TypeVar('S')

class _PageEventLoop(Thread):
    __stop: threading.Event
    __path: str
    __port: int
    __subscription: Dict[str, Dict[str, List[str]]]
    __with_event: Tuple[Callable[[PageEvent], None]]

    def __init__(self,
                 stop: threading.Event,
                 path: str,
                 subscription: Dict[str, Dict[str, List[str]]],
                 with_event: Callable[[PageEvent], None],
                 port: int = MYXINE_DEFAULT_PORT) -> None:
        super().__init__()
        self.__with_event = (with_event,)
        self.__subscription = subscription
        self.__port = port
        self.__path = path
        self.__stop = stop

    def run(self) -> None:
        for e in subscribe(self.__path, self.__subscription, port=self.__port):
            self.__with_event[0](e)
            print(e)
            # if self.__stop.is_set(): break

class Page(Generic[S], Thread):
    __state: S
    __page_hooks: Dict[str,
                       Dict[str,
                            Tuple[Set[str],
                                  List[Tuple[List[str],
                                             Callable[[S, List[Optional[Any]]], bool]]]]]] = {}
    __custom_hooks: Dict[str, List[Callable[[S, Any], bool]]] = {}
    __render: Tuple[Callable[[S], Tuple[Optional[str], str]]] = ((lambda s: (None, '')),)
    __queue: SimpleQueue = SimpleQueue()
    __path: str = '/'
    __port: int = MYXINE_DEFAULT_PORT
    __running_lock: Lock = Lock()
    __running: bool = False
    __settings_lock: Lock = Lock()

    def __init__(self, *, state: S) -> None:
        super().__init__()
        self.__state = state

    def render_with(self, render: Callable[[S], Tuple[Optional[str], str]]) -> None:
        with self.__settings_lock:
            self.__render = (render,)

    def port(self, port: int) -> None:
        # Prevent race conditions on running page
        with self.__running_lock:
            if self.__running:
                raise ValueError("Can't change the port of a running page")
            self.__port = port

    def path(self, path: str) -> None:
        # Prevent race conditions on running page
        with self.__running_lock:
            if self.__running:
                raise ValueError("Can't change the path of a running page")
            self.__path = path

    def on(self,
           event: str, target: str, results: List[str],
           do: Callable[[S, List[Optional[Any]]], bool]) -> None:
        with self.__settings_lock:
            # Drill down to the correct position
            if self.__page_hooks.get(target) is None:
                self.__page_hooks[target] = {}
            if self.__page_hooks[target].get(event) is None:
                self.__page_hooks[target][event] = (set(), [])
            # Add our hook
            (all_results, all_hooks) = self.__page_hooks[target][event]
            for result in results:
                all_results.add(result)
            all_hooks.append((results, do))

    def on_notify(self, event: str, do: Callable[[S, Any], bool]) -> None:
        with self.__settings_lock:
            event_hooks = self.__custom_hooks.get(event)
            if event_hooks is None: event_hooks = []
            event_hooks.append(do)
            self.__custom_hooks[event] = event_hooks

    def notify(self, event: str, data: Any) -> None:
        self.__queue.put((event, None, data))

    def stop(self) -> None:
        self.__queue.put(None)

    def __subscription(self) -> Dict[str, Dict[str, List[str]]]:
        subscription: Dict[str, Dict[str, List[str]]] = {}
        for target, event_hooks in self.__page_hooks.items():
            if subscription.get(target) is None:
                subscription[target] = {}
            for event, (results, _) in event_hooks.items():
                if subscription[target].get(event) is None:
                    subscription[target][event] = []
                subscription[target][event].extend(list(results))
        print(subscription)
        return subscription

    def run(self) -> None:
        # Make sure we don't run the page concurrently with itself
        with self.__running_lock:
            if not self.__running: self.__running = True
            else: raise ValueError('Page is already running')
        stop_event_loop = threading.Event()
        event_loop = \
            _PageEventLoop(stop_event_loop,
                           self.__path,
                           self.__subscription(),
                           lambda e: self.__queue.put((e.event(), e.target(), e)),
                           port=self.__port)
        event_loop.start()
        (title, body) = self.__render[0](self.__state)
        update(self.__path, body, title, port=self.__port)
        while True:
            item = self.__queue.get()
            if item is None: break # caused by the 'stop' method
            should_render = False
            (event, target, data) = item
            if target is None:
                hooks = self.__custom_hooks.get(event)
                if hooks is not None:
                    for hook in hooks:
                        should_render = should_render or hook(data, self.__state)
            else:
                try:
                    for arg_names, hook in self.__page_hooks[target][event][1]:
                        args = []
                        for arg_name in arg_names:
                            args.append(data[arg_name])
                        should_render &= hook(self.__state, args)
                except KeyError: pass
            # if should_render:
            (title, body) = self.__render[0](self.__state)
            print(body)
            update(self.__path, body, title, port=self.__port)
        stop_event_loop.set() # this won't kill it immediately, but it will
                              # stop at the next heartbeat from the server
        with self.__running_lock:
            self.__running = False
