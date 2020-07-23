from typing import Optional, Iterator, Dict, List, Any
import requests
from dataclasses import dataclass
from requests import RequestException
import json
from copy import deepcopy
import urllib.parse
from semantic_version import Version, SimpleSpec

# The default port on which myxine operates; can be overridden in the below
# functions if the server is running on another port.
DEFAULT_PORT = 1123


# The supported versions of the myxine server
SUPPORTED_SERVER_VERSIONS = SimpleSpec('>=0.2,<0.3')


# The global session for all requests
__GLOBAL_SESSION = requests.Session()


@dataclass
class Target:
    """A Target corresponds to an element in the browser's document. It
    contains a tag name and a mapping from attribute name to attribute value.
    """
    tag: str
    attributes: Dict[str, str]


class Event:
    """An Event from a page has a type, a list of targets, and a set of
    properties keyed by strings, which may be any type. All properties of an
    event are accessible as fields of this object, though different event types
    may have different sets of fields.
    """
    __event: str
    __targets: List[Target]
    __properties: Dict[str, Any]
    __finalized: bool = False

    def __getattr__(self, name: str) -> Any:
        value = self.__properties[name]
        if value is None:
            raise AttributeError
        return value

    def __setattr__(self, name: str, value: Any) -> None:
        if self.__finalized:
            raise ValueError("Event objects are immutable once created")
        super(Event, self).__setattr__(name, value)

    def __dir__(self) -> List[str]:
        fields = dir(super(Event, self)) + \
            list(self.__properties.keys()) + \
            ['event', 'targets']
        return sorted(set(fields))

    def event(self) -> str:
        """Returns the event name for this event."""
        return self.__event

    def targets(self) -> List[Target]:
        """Returns the list of targets for this event, in order from most to
        least specific in the DOM tree."""
        return deepcopy(self.__targets)

    def __init__(self, value: Dict[str, Any]) -> None:
        """Parse a JSON-encoded event. Returns None if it can't be parsed."""
        try:
            self.__event = value['event']
            self.__targets = [Target(tag=j['tagName'],
                                     attributes=j['attributes'])
                              for j in value['targets']]
            self.__properties = value['properties']
        except json.JSONDecodeError:
            raise ValueError("Could not parse event: " + str(value)) from None
        except KeyError:
            raise ValueError("Could not parse event: " + str(value)) from None
        self.__finalized = True


def page_url(path: str, port: int = DEFAULT_PORT) -> str:
    """Normalize a port & path to give the localhost url for that location."""
    if len(path) > 0 and path[0] == '/':
        path = path[1:]
    return 'http://localhost:' + str(port) + '/' + path


def events(path: str,
           subscription: Optional[List[str]] = None,
           port: int = DEFAULT_PORT,
           ignore_server_version: bool = False) -> Iterator[Event]:
    """Subscribe to a stream of page events from a myxine server, returning an
    iterator over the events returned by the stream as they become available.
    """
    base_url = page_url(path, port)
    try:
        # The base parameters of the request
        params: Dict[str, Any]
        if subscription is None:
            params = {'events': ''}
        else:
            params = {'events': subscription}
        params['next'] = ''  # The first time around, /?next&events=...

        # The earliest event we will be willing to accept
        moment: str = ''

        while True:
            url = urllib.parse.urljoin(base_url, moment)
            response = __GLOBAL_SESSION.get(url, params=params)
            if response.encoding is None:
                response.encoding = 'utf-8'
            if not ignore_server_version:
                check_server_version(response)
                ignore_server_version = True
            event = Event(response.json())
            if event is not None:
                yield event

            # Set up the next request
            moment = response.headers['Content-Location']

    except RequestException:
        msg = "Connection issue with myxine server (is it running?)"
        raise ConnectionError(msg) from None


def evaluate(path: str, *,
             expression: Optional[str] = None,
             statement: Optional[str] = None,
             port: int = DEFAULT_PORT,
             ignore_server_version: bool = False) -> None:
    """Evaluate the given JavaScript code in the context of the page."""
    bad_args_err = \
        ValueError('Input must be exactly one of a statement or an expression')
    if expression is not None:
        if statement is not None:
            raise bad_args_err
        url = page_url(path, port)
        params = {'evaluate': expression}
        data = b''
    elif statement is not None:
        if expression is not None:
            raise bad_args_err
        url = page_url(path, port) + '?evaluate'
        params = {}
        data = statement.encode()
    else:
        raise bad_args_err
    try:
        r = __GLOBAL_SESSION.post(url, data=data, params=params)
        if not ignore_server_version:
            check_server_version(r)
        if r.status_code == 200:
            return r.json()
        raise ValueError(r.text)
    except RequestException:
        msg = "Connection issue with myxine server (is it running?)"
        raise ConnectionError(msg) from None


def update(path: str,
           body: str,
           title: Optional[str] = None,
           port: int = DEFAULT_PORT,
           ignore_server_version: bool = False) -> None:
    """Set the contents of the page at the given path to a provided body and
    title. If body or title is not provided, clears those elements of the page.
    """
    url = page_url(path, port)
    try:
        params = {'title': title}
        r = __GLOBAL_SESSION.post(url, data=body.encode(), params=params)
        if not ignore_server_version:
            check_server_version(r)
    except RequestException:
        msg = "Connection issue with myxine server (is it running?)"
        raise ConnectionError(msg) from None


def static(path: str,
           body: bytes,
           content_type: str,
           port: int = DEFAULT_PORT,
           ignore_server_version: bool = False) -> None:
    """Set the contents of the page at the given path to the static content
    provided, as a bytestring. You must specify a content type, or else the
    browser won't necessarily know how to display this content.
    """
    url = page_url(path, port) + '?static'
    try:
        headers = {'Content-Type': content_type}
        r = __GLOBAL_SESSION.post(url, data=body, headers=headers)
        if not ignore_server_version:
            check_server_version(r)
    except RequestException:
        msg = "Connection issue with myxine server (is it running?)"
        raise ConnectionError(msg) from None


def check_server_version(response: requests.Response) -> None:
    """Check to make sure the Server header in the given response is valid for
    the versions supported by this version of the client library, and throw an
    exception if not.
    """
    try:
        server_version = response.headers['server']
        if server_version is not None:
            try:
                server, version_string = server_version.split('/')
                if server == "myxine":
                    try:
                        version = Version.coerce(version_string)
                        if version in SUPPORTED_SERVER_VERSIONS:
                            return
                        else:
                            msg = f"Unsupported version of the myxine server: \
                            {version}; supported versions are {str(SUPPORTED_SERVER_VERSIONS)}"
                            raise ConnectionError(msg) from None
                    except ValueError:
                        msg = f"Could not parse myxine server version string: {version_string}"
                        raise ConnectionError(msg) from None
            except ValueError:
                pass
    except KeyError:
        pass

    # Default:
    msg = "Server did not identify itself as a myxine server."
    raise ConnectionError(msg) from None
