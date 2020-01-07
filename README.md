# `myxine`: a slithery sea-friend to help you _get GUI fast_

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

If you can write a function (in any language) from some internal data to a
fragment of HTML, `myxine` will give your program a dynamic webpage whose
content [instantly reflects](#lets-play) whatever you'd like it to show. You can
then [listen to events](#interactivity) within that page to quickly prototype a
reactive user interface, with only a knowledge of HTML and your favorite
programming language.

**Q:** Could you show me something cool, then tell me the details after?<br/>
**A:** Happily, [let's get started](#show-me)!

**Q:** I want to know all about how it works, then can you show me the demo after? <br/>
**A:** Sure thing, [let's dig in](#lets-play)!

## Show me!

First, [install `myxine`](#installation), get a cup of tea while it builds, and
then come back here :)

Second, make sure you have [Python 3](https://www.python.org/) and the
[`requests`](https://2.python-requests.org/en/master/) library installed. Myxine
doesn't depend on them, but we'll need them to run this example. If you have
Python 3 (and therefore hopefully [`pip3`](https://pypi.org/project/pip/)) on
your system, you can install `requests` with:

```bash
$ pip3 install requests
```

Now, in one terminal window, run:

```bash
$ myxine
```

And in another window, run:

```bash
$ ./examples/angles.py
```

Then open up [http://localhost:1123/](http://localhost:1123/) in your web
browser, and mouse around! If you like, see if you can figure out what's going
on by reading [the Python source for this example](/examples/angles.py), or read
on to learn more now...

## Getting started

### Installation

You will need a recent version of the Rust programming langauge and its build
tool, `cargo`. If you don't have that, [here's the quick-start for installing
Rust](https://www.rust-lang.org/learn/get-started). Once you have `cargo`
installed, run:

```bash
$ cargo install
```

### Running

Myxine is meant to run in the background. It might live longer than any
individual program that uses it, and it's meant to be a service many programs
might use at the same time. To get started, run:

```bash
$ myxine
```

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

### Step 1: Say what you want

Write down a *subscription* consisting of some [JSON](https://www.json.org/)
specifying which events you care about. The format should be a doubly-nested
dictionary like this:

```json
{
  "#some-id": {
    "click": [".x", ".y"]
  },
  "window": {
    "keydown": [".key"],
    "keyup": [".key"]
  },
  "document.body": {
    "scroll": ["window.scrollY"]
  }
}
```

The outer dictionary maps each *event target* of interest in the document to a
dictionary which maps each *event name* of interest for that target to a list of
desired *return values*. Targets are named either by HTML element `id`,
specified using a preceding `#`, or by name within the page's environment of
objects, as a sequence of field selections.

When the event occurs on the target, `myxine` will look up and send back the
value of all the return values you asked for. This list can contain global
properties specified like `window.scrollY` or properties of the current event
specified with a preceding `.` like `.key`.

MDN web docs has [excellent documentation about browser
events](https://developer.mozilla.org/en-US/docs/Web/Events), which I recommend
as a reference for finding the names and properties of the browser events to
which you can subscribe.

### Step 2: Get what you want

Now that you have a description of the events you
want to listen for, send a **POST** request to the desired page path, with the
subscription as the body of the request and `?subscribe` as the query string.
With `curl`, this looks like:

```bash
$ curl 'localhost:1123/some/path?subscribe' -d '{ "window": "click": [".x", ".y"] }'
id: window
event: click
data: {".x":638,".y":757}

id: window
event: click
data: {".x":796,".y":1334}

:

id: window
event: click
data: {".x":749,".y":523}
```

This will return an endless stream of events in the [`text/event-stream`
format](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#Event_stream_format),
a line-based text format for streams of events with attached data. In `myxine`'s
case, every event will have an `id`, an `event`, and some attached `data`
formatted as a JSON dictionary mapping result fields you asked for to the value
they had at the time the event occurred in the page. Occasionally, the stream
will also contain an empty "heartbeat" message `:` which `myxine` uses to check
that you're still listening—you can ignore these.

### Step 3: Interact!

For an example of an interactive page using event subscriptions, check out [the
`angles` example in Python](/examples/angles.py). Make sure `myxine` is running,
then run:

```bash
$ ./examples/angles.py`
```

Then load up [http://localhost:1123/](http://localhost:1123/) and mouse around!

If your language doesn't implement a parser for this format, check out [the
17-line Python implementation](/examples/myxine.py#L14-L36) as a reference. Feel
free to submit a pull request with an implementation in your language, or to
publish it yourself somewhere! For the technical details I used when writing
this parser, see [the W3C Recommendation for Server-Sent
Events](https://www.w3.org/TR/eventsource/) and look at the sections for
[parsing](https://www.w3.org/TR/eventsource/#parsing-an-event-stream) and
[interpretation](https://www.w3.org/TR/eventsource/#event-stream-interpretation).
You can ignore everything about what to do "as a user-agent" because you are not
a user-agent :)
