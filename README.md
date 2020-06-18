# Myxine: a slithery sea-friend to help you _get GUI fast_

<table style="border: 0">
<tr style="border: 0">
  <td width="40%" style="border: 0">
    <img src="/images/myxine_glutinosa.png" target="_blank" width="425px" alt="woodcut sketch of myxine glutinosa, the hagfish">
  </td>
  <td style="border: 0">
    <p>Hagfish, a.k.a. <a href="https://en.wikipedia.org/wiki/Hagfish"><i>myxine glutinosa</i></a>, are eel-like sea creatures best known for their ability to make a lot of slime.</p/>
    <p>By analogy, <code>myxine</code> quickly reduces the friction in creating a dynamic graphical interface, helping you to <b><i>get GUI fast</i></b> in any language under the sea.</p>
  </td>
</tr>
</table>

## TL;DR:

If you write a function in *any programming language* that makes some HTML,
Myxine can give you a dynamic webpage whose content [instantly
reflects](#lets-play) whatever you'd like it to show. You can then [listen to
events](#interactivity) within that page to quickly prototype a reactive user
interface—with only a knowledge of HTML and your favorite language.

More precisely: `myxine` is a local server that gives you a RESTful API for
creating interactive applications in your web browser, using any programming
language capable of sending an HTTP request.

The intent of `myxine` is to make it easy and succinct to write idiomatic
bindings to it in any programming language, or to use it from the shell
directly. Currently, there are bindings for Python and Haskell, and work in
progress toward bindings for Rust, JavaScript, and more.

## Getting started

### Installing and running

You will need a recent version of the Rust programming langauge and its build
tool, `cargo`. If you don't have that, [here's the quick-start for installing
Rust](https://www.rust-lang.org/learn/get-started). Once you have `cargo`
installed, install the latest version of `myxine`:

```bash
$ cargo install myxine
```

Then, start it in the background and leave it running:

```bash
$ myxine
Running at: http://127.0.0.1:1123
```

By default, `myxine` uses the port 1123, but you can pick a different one using
the `--port` command line option.

### Let's play!

Myxine speaks to the world through HTTP requests and responses. If you can make
a web request to `localhost` from your program, you can use `myxine`.

Open your browser to [http://localhost:1123/](http://localhost:1123/), then
watch what happens when you run this command in your terminal:

```bash
$ curl 'localhost:1123/' \
       -d '<h1 style="color: blue; padding: 20pt; font-family: Helvetica">
             Splish splash!
           </h1>'
```

**What's going on:**

1. If you **POST** some HTML to `localhost:1123/some/arbitrary/path`, and then
2. **GET** (i.e. navigate with your web browser) from
   `localhost:1123/some/arbitrary/path`: you'll see a web page with the HTML
   fragment you just posted set as the contents of its `<body>`.
3. When you **POST** some more HTML to that same path, the changes will be
   instantly updated on the web page.

Some more things you can do:

- **Set the page title:** use the `?title` query parameter, like this:

    ```bash
    $ curl 'localhost:1123/?title=Hello%20Atlantic%20Ocean!' \
           -d '<h1 style="color: blue; padding: 20pt; font-family: Helvetica">
                  What a fine day it is!
               </h1>'
    ```

    Titles will be URL-decoded, so you can use, e.g. `%20` to put a space in your title.

- **Store static content:** You can store other kinds of data with `myxine`
  (such as assets you want to link to). If you append to your request path the
  query parameter `?static`, `myxine` will interpret your data as raw bytes, and
  forego injecting them into an interactive page. For best results, set the
  `Content-Type` header of your request so `myxine` knows what kind of data to
  tell your browser it's receiving.

  To publish a static piece of JSON data with `curl`, you might say:

  ```bash
  $ curl -H "Content-Type: application/json"   \
         'localhost:1123/swimming.json?static' \
         -d '{ "splish": "splash" }'
  ```

  You can still update the content with further `POST` requests, but a web browser
  won't see those changes until you reload the page.

- **Store binary files:** A common gotcha is trying to upload non-text content
  but forgetting to send it in binary mode—this will corrupt your data in
  transmission. To make sure non-text things get transmitted okay, make sure you
  send the request in binary mode. For example, to upload an image `ocean.png`
  with `curl`, you could say:

  ```bash
  $ curl -H "Content-Type: image/png"      \
         'localhost:1123/ocean.png?static' \
         --data-binary @"ocean.png"
  ```

## Interactivity

Interfaces are meant to be *interactive*: Myxine lets you listen to events
happening in the page without writing a lick of JavaScript.

### Supported events

There are many kinds of user-interface events which can happen in the browser.
Most of them are supported by `myxine`, but not all. Those which aren't usually
are one or more of:

- "non-bubbling" events which need to be attached to a specific element rather
  than the document as a whole
- high-frequency repeated events that fire continuously (and therefore would be
  an automatic performance problem)
- events which are difficult to test support for using the hardware I have
  available as a developer

The master list of supported events is programmatically defined in the JSON file
[enabled-events.json](enabled-events.json), and the hierarchy of events and
their interfaces can be visualized in [this clickable
graphic](https://raw.githubusercontent.com/GaloisInc/myxine/master/enabled-events.svg?sanitize=true).
This file defines a subset of the [standardized DOM
events](https://developer.mozilla.org/en-US/docs/Web/Events) in JavaScript, as
well as the inheritance hierarchy for the interfaces of those events and the
fields which are to be reported for each event interface. This list is
intentionally conservative: if you are in need of support for another event or
set of events, feel free to submit a PR with changes to this file.

### Event format

Events are returned as linebreak- and whitespace-free JSON dictionaries. When
streaming, consecutive events are separated by newlines. Each event dictionary
is of the form:

- `event`: a *string* giving the name of the event according to JavaScript
- `targets`: a *list* of *dictionaries* representing an element in the DOM upon
  which the event fired, in order from innermost to outermost, each of which is
  of the form:
  - `tagName`: a *string* giving the (lowercase) HTML tag name of the element
  - `attributes`: a *dictionary* mapping the element's HTML attribute names to
    their corresponding *string* values
- `properties`: a *dictionary* of attributes of the event itself, whose names
  and value types vary depending on the type of event

For example, consider this event (formatted with spaces and newlines for reading
convenience):

``` json
{
    "event": "click",
    "targets": [
        {"tagName": "button", "attributes": {"id": "abc"}},
        {"tagName": "body", "attributes": {"class": "xyz"}},
        {"tagName": "html", "attributes": {}}
    ],
    "properties": {
        "altKey": false,
        "button": 0,
        "buttons": 0,
        "clientX": 911,
        "clientY": 195,
        "ctrlKey": false,
        "detail": 1,
        "metaKey": false,
        "movementX": 0,
        "movementY": 0,
        "offsetX": 911,
        "offsetY": 195,
        "pageX": 911,
        "pageY": 195,
        "screenX": 749,
        "screenY": 1914,
        "shiftKey": false
    }
}
```

This represents a `click` event on an element `<button id="abc" />` contained in
an element `<body class="xyz">` contained in the `<html>` page, with a variety
of properties describing the location of the click, the state of keys being held
down during the click, and other information. See [above](#supported-events) for
resources to aid your understanding of these properties.

### Listening to events

There are two distinct APIs for accessing the events on a page:

- **Streaming:** You can make a single request and receive an unending
  newline-separated stream of events. One caveat: because this is a
  line-oriented streaming HTTP body, your language of choice must allow
  non-buffered access to the raw stream in order for this API to be useful. If
  not, the long-polling API may be more suitable.
- **Long-polling:** You can issue one request for each event you wish to
  receive, and wait until such an event is available. Although it might seem
  that repeated polling could run the risk of dropping events, Myxine will take
  care of buffering events for you to ensure you will receive each event you
  want in sequence. This API is slightly more complex than the streaming API, as
  it requires you to keep track of the "moment in time" corresponding to the
  latest event you received.

#### Streaming events

To listen to a stream of the events happening in a page, send a **GET** request
to that page's URL, with the query flag `?stream` and a set of query parameters
describing which events you wish to receive. If you don't specify any events,
every page event will be included in the stream. Otherwise, you can ask for only
specific events to be returned by listing their names using the `?event=...`
query parameter (this may be repeated).

Using `curl`, this might look like below:

```bash
$ curl 'localhost:1123/some/path/?stream&event=click&event=keydown'
{"event":"click","targets":[{"attributes":{},"tagName":"html"}],"properties":{"altKey":false,"button":0,"buttons":0,"clientX":911,"clientY":195,"ctrlKey":false,"detail":1,"metaKey":false,"movementX":0,"movementY":0,"offsetX":911,"offsetY":195,"pageX":911,"pageY":195,"screenX":749,"screenY":1914,"shiftKey":false}}
{"event":"click","targets":[{"attributes":{},"tagName":"html"}],"properties":{"altKey":false,"button":0,"buttons":0,"clientX":1180,"clientY":338,"ctrlKey":false,"detail":1,"metaKey":false,"movementX":0,"movementY":0,"offsetX":1180,"offsetY":338,"pageX":1180,"pageY":338,"screenX":964,"screenY":2028,"shiftKey":false}}
{"event":"keydown","targets":[{"attributes":{"style":"margin: 0px; padding: 0px"},"tagName":"body"},{"attributes":{},"tagName":"html"}],"properties":{"altKey":false,"code":"KeyA","ctrlKey":false,"detail":0,"isComposing":false,"key":"a","metaKey":false,"repeat":false,"shiftKey":false}}
{"event":"keydown","targets":[{"attributes":{"style":"margin: 0px; padding: 0px"},"tagName":"body"},{"attributes":{},"tagName":"html"}],"properties":{"altKey":false,"code":"KeyB","ctrlKey":false,"detail":0,"isComposing":false,"key":"b","metaKey":false,"repeat":false,"shiftKey":false}}
{"event":"click","targets":[{"attributes":{},"tagName":"html"}],"properties":{"altKey":false,"button":0,"buttons":0,"clientX":1180,"clientY":338,"ctrlKey":false,"detail":1,"metaKey":false,"movementX":0,"movementY":0,"offsetX":1180,"offsetY":338,"pageX":1180,"pageY":338,"screenX":964,"screenY":2028,"shiftKey":false}}
```

#### Long-polling for events

Alternatively, you can repeatedly query for the next event matching a set of
desired events, receiving an instant response if such an event has already
happened without your having received it---or a response delayed until such an
event occurs, if no such unseen event has yet happened.

How it works: the Myxine server has an internal count of how many events have
*ever* occurred in the lifetime of the page in question. When you make a request
for an event, the `Content-Location` header of the response indicates a URL on
the server which corresponds to the that event's unique sequence number. By
directing your next query to that location, you inform the server to hand you
the next event after that one---and by continuing to follow the links in the
`Content-Location` headers, you skip forward one event at a time in the sequence
of events that occurred in the page.

Let's see an example. We'll use the `-v` mode for `curl`, which shows us the
request and response headers, as well as other meta-information.

To start, we make a request for the next event by using the `?next` query flag:

```bash
$ curl -v 'localhost:1123/some/path/?next&event=focus&event=keydown'
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 1123 (#0)
> GET /some/path/?next HTTP/1.1
> Host: localhost:1123
> User-Agent: curl/7.64.1
> Accept: */*
>
< HTTP/1.1 200 OK
< cache-control: no-cache
< content-type: application/json; charset=utf8
< content-location: /some/path/?after=227
< content-length: 47
< date: Thu, 18 Jun 2020 17:15:43 GMT
<
{"event":"focus","targets":[],"properties":{}}
* Connection #0 to host localhost left intact
* Closing connection 0
```

Notice the line:

```bash
< content-location: /some/path/?after=227
```

If we request that exact path, we will receive the same event. In other words,
the parameter `?after=227` represents the canonical reference to this particular
event, and we can retrieve it as many times as we like (until it leaves the
server's buffer):

```bash
$ curl 'localhost:1123/some/path/?after=227'
{"event":"focus","targets":[],"properties":{}}
```

To get the next event, however, we again use the `?next` query flag and repeat
our request for the specific events we're interested in (in this case, `focus`
and `keydown`):

```bash
$ curl -v 'localhost:1123/some/path/?after=227&next&event=focus&event=keydown'
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 1123 (#0)
> GET /some/path/?after=227&next&event=focus&event=keydown HTTP/1.1
> Host: localhost:1123
> User-Agent: curl/7.64.1
> Accept: */*
>
< HTTP/1.1 200 OK
< cache-control: no-cache
< content-type: application/json; charset=utf8
< content-location: /some/path/?after=228
< content-length: 291
< date: Thu, 18 Jun 2020 17:51:07 GMT
<
* Connection #0 to host localhost left intact
{"event":"keydown","targets":[{"attributes":{"style":"margin: 0px; padding: 0px"},"tagName":"body"},{"attributes":{},"tagName":"html"}],"properties":{"altKey":false,"code":"MetaLeft","ctrlKey":false,"detail":0,"isComposing":false,"key":"Meta","metaKey":true,"repeat":false,"shiftKey":false}}* Closing connection 0
```

Notice the line:

```bash
< content-location: /some/path/?after=228
```

If we were to continue this example, we would query again starting at this
point---but hopefully you get the rhythm of the thing by now!

**One more thing:** Keep in mind that although the server has a generously sized
event buffer, if you lag too far behind in requests, the event you're looking
for may already have been evicted from the server's buffer. In this case, the
server will issue a **308: Permanent Redirect** response pointing to the
earliest known event matching the request, and noting in human-readable format
how far behind the buffer's tail the request fell:

``` bash
$ curl -v 'localhost:1123/some/path/?after=0'
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 1123 (#0)
> GET /some/path/?after=0 HTTP/1.1
> Host: localhost:1123
> User-Agent: curl/7.64.1
> Accept: */*
>
< HTTP/1.1 308 Permanent Redirect
< cache-control: no-cache
< location: /some/path/?after=391
< content-length: 36
< date: Thu, 18 Jun 2020 17:57:19 GMT
<
* Connection #0 to host localhost left intact
Client request lagged by 391 events.* Closing connection 0
```

If we follow redirects (in `curl` this means using the `-L` flag), the server
will return this event we asked for, and we can again continue using the
`Content-Location` header field to pick up the sequence of events:

``` bash
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 1123 (#0)
> GET /some/path/?after=0 HTTP/1.1
> Host: localhost:1123
> User-Agent: curl/7.64.1
> Accept: */*
>
< HTTP/1.1 308 Permanent Redirect
< cache-control: no-cache
< location: /some/path/?after=396
< content-length: 36
< date: Thu, 18 Jun 2020 18:01:30 GMT
<
* Ignoring the response-body
* Connection #0 to host localhost left intact
* Issue another request to this URL: 'http://localhost:1123/some/path/?after=396'
* Found bundle for host localhost: 0x7fe220c1c000 [can pipeline]
* Could pipeline, but not asked to!
* Re-using existing connection! (#0) with host localhost
* Connected to localhost (127.0.0.1) port 1123 (#0)
> GET /some/path/?after=396 HTTP/1.1
> Host: localhost:1123
> User-Agent: curl/7.64.1
> Accept: */*
>
< HTTP/1.1 200 OK
< cache-control: no-cache
< content-type: application/json; charset=utf8
< content-location: /some/path/?after=396
< content-length: 319
< date: Thu, 18 Jun 2020 18:01:30 GMT
<
* Connection #0 to host localhost left intact
{"event":"mousemove","targets":[{"attributes":{},"tagName":"html"}],"properties":{"altKey":false,"button":0,"buttons":0,"clientX":745,"clientY":733,"ctrlKey":false,"detail":0,"metaKey":false,"movementX":86,"movementY":30,"offsetX":746,"offsetY":733,"pageX":745,"pageY":733,"screenX":616,"screenY":708,"shiftKey":false}}* Closing connection 0
```

The default in most HTTP client libraries is to transparently follow redirects,
which means by default, you'll just miss events if you lag too far behind the
server's buffer. In the cases where you might want to alter your client code's
behavior based on the fact that lag occurred, your HTTP client library of choice
likely offers some options for inspecting the response to determine how many
redirects were followed. If this is more than zero, then your client lagged
behind the server's buffer.

## The escape hatch: evaluating arbitrary JavaScript

It occasionally might become necessary for you to directly evaluate some
JavaScript within the context of the page. The most frequent reason for this is
to query the value of some object, such as the current contents of a text-box,
or the current window dimensions. To allow this, `myxine` exposes a simple API
to send arbitrary JavaScript to the page and return its result: the `?evaluate`
query string.

There are two ways to use this API, corresponding to JavaScript's notions of
"expression" and "statement". The more convenient of the two evaluates a given
string as an *expression* and returns its value:

```bash
$ curl -X POST "http://localhost:1123/?evaluate=window.innerWidth"
1224
```

This form is succinct: you don't have to use JavaScript's `return` keyword, and
you can specify everything in the URL itself. However, you can't evaluate
multiple lines delimited by semicolons (since the input is interpreted as an
expression), and you must percent-escape all special characters like spaces.

To circumvent these limitations, `myxine` also provides a "statement" form of
the `?evaluate` API, where the POST body is used as a multi-line block of
statements to be evaluated:

```bash
$ curl "http://localhost:1123/?evaluate" -d \
    'let x = 100; let y = 200; return x + y;'
300
```

In this form, `return` is mandatory to send back a value, but there is no need
to escape special characters, and multiple statements can be executed as a
block.

### Further details

- Return values of `undefined` are reported as `null`, and therefore calls which
  don't return anything (i.e. if you did not use `return` in a statement-type
  request) result in `null`.
- Return types are limited to those which can be serialized via the JavaScript
  method
  [`JSON.stringify`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify),
  which does not work for cyclic objects (like `window`, `document`, and all DOM
  nodes), and may fail to serialize some properties for other non-scalar values.
  If you want to return a non-scalar value like a list or dictionary, construct
  it explicitly yourself by copying from the fields of the object you're
  interested in.
- There is a default timeout of 1 second for reporting evaluation results. This
  is not enforced in the browser -- your JavaScript will continue to run
  indefinitely -- but is a guarantee of the `myxine` API. To alter this timeout,
  use the `timeout` query parameter to specify the desired limit in
  milliseconds.
- All errors in evaluation, including timeouts, serialization errors, and other
  exceptions, are caught and reported with status code `400 Bad Request`. If the
  status code is `200 OK`, it's guaranteed that the response will be valid JSON.
  Otherwise, the response body is a human-readable description of the error.
- For both forms, the JavaScript given is evaluated in the `window` (global)
  scope of the browser. For nitty-gritty details, see the docs on
  [`window.Function`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function)
  and the MDN writeup
  ["Never use `eval()`!"](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval#Never_use_eval!).
