# Myxine: a slithery sea-friend to help you _get GUI fast_

<table style="border: 0">
<tr style="border: 0">
  <td width="40%" style="border: 0">
    <img src="/images/myxine_glutinosa.png" target="_blank" width="425px" alt="woodcut sketch of myxine glutinosa, the hagfish">
  </td>
  <td style="border: 0">
    <p>Hagfish, a.k.a. <a href="https://en.wikipedia.org/wiki/Hagfish"><i>myxine glutinosa</i></a>, are an eel-like sea creatures best known for their ability to make a lot of slime.</p/>
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
creating model-view-controller style interactive applications in your browser
from any programming language capable of sending an HTTP request.

The intent of `myxine` is to make it easy and succinct to write idiomatic
bindings to it in any programming language, or to use it from the shell
directly. Currently, there are bindings for Python and Haskell, and work in
progress toward bindings for Rust, JavaScript, and more.

**Q:** Could you show me something cool, then tell me the details after?<br/>
**A:** Happily, [let's get started](#show-me)!

**Q:** I want to know all about how it works, then can you show me the demo after? <br/>
**A:** Sure thing, [let's dig in](#getting-started)!

## Show me!

First, [install `myxine`](#installation) -- in short:

```bash
$ cargo install myxine
  ... (things get compiled) ...
$ myxine
Running at: http://127.0.0.1:1123
```

Second, make sure you have [Python 3](https://www.python.org/) and the
[`requests`](https://2.python-requests.org/en/master/) library installed. Myxine
doesn't itself depend on them, but we'll need them presently because this example
happens to be written in Python. If you have Python 3 (and therefore hopefully
[`pip3`](https://pypi.org/project/pip/)) on your system, you can install
`requests` with:

```bash
$ pip3 install requests
```

And in another window, run:

```bash
$ ./examples/circles.py
```

Then open up [http://localhost:1123/](http://localhost:1123/) in your web
browser, and mouse around! See if you can figure out what's going on by reading
[the Python source for this example](/examples/circles.py), or read on for [the
full story...](#lets-play)

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
   instantly updated on the web page before your eyes!

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

Interfaces are meant to be *interactive*: `myxine` lets you listen to events
happening in the page without writing a lick of JavaScript.

### Listening to the event stream

To listen to the events happening in a page, send a **GET** request to that
page's URL, with the query string `?events`. Using `curl`, this might look like
below (some data has been elided for brevity).

```bash
$ curl 'localhost:1123/some/path?events'
id: [{"attributes":{},"tagName":"html"}]
event: mousemove
data: {"x":352,"y":237, ... }

id: []
event: blur
data: {}

:

id: []
event: focus
data: {}

id: [{"attributes":{"style":"margin: 0px; padding: 0px"},"tagName":"body"},{"attributes":{},"tagName":"html"}]
event: keydown
data: {"altKey":false,"ctrlKey":false,"key":"f","metaKey":false,"shiftKey":false, ... }
```

This will return an endless stream of events in the [`text/event-stream`
format](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#Event_stream_format),
a line-based text format for streams of events with attached data.

### Understanding events

In `myxine`'s case, every event will have:

1. `id`: A JSON list of "target" objects identifying the path from the most
   specific location of the event in the document to the least specific. Each
   target object in this path has a `tagName` string identifying the HTML tag of
   the corresponding element, and an `attributes` dictionary giving the value of
   each attribute of that element. The list of targets may be empty, in which
   case it corresponds to an event that fired directly on some top-level object
   in the browser (as is the case for the `blur` and `focus` events in the above
   example).
2. `event`: The name of the JavaScript event to which this item corresponds.
3. `data`: A JSON dictionary holding the properties of the event. Different
   events have different sets of properties associated with them, so the
   contents of this dictionary may vary depending on the event you are
   examining.

So, for instance, you click on an element `<div id="something", class="cool"></div>`, the corresponding event will look something like:

```
id: [{"tagName":"div","attributes":{"id":"something","class":"cool"}}, ... ]
event: click
data: {"x":352,"y":237, ... }
```

If your language doesn't implement a parser for this format, check out [the
17-line Python implementation](/examples/myxine.py#L14-L36) as a reference. For
the technical details I used when writing this parser, see [the W3C
Recommendation for Server-Sent Events](https://www.w3.org/TR/eventsource/) and
look at the sections for
[parsing](https://www.w3.org/TR/eventsource/#parsing-an-event-stream) and
[interpretation](https://www.w3.org/TR/eventsource/#event-stream-interpretation).
You can ignore everything about what to do "as a user-agent" because you are not
a user-agent :)

**Example:** For an example of an interactive page using event subscriptions,
check out [the `circles` example in Python](/examples/circles.py). Make sure
`myxine` is running, then run:

```bash
$ ./examples/circles.py
```

Then load up [http://localhost:1123/](http://localhost:1123/) and mouse around!

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
