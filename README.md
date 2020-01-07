# `myxine`: a slithery sea-friend to help you _get GUI fast_

<table style="border: 0">
<tr style="border: 0">
  <td width="40%" style="border: 0">
    <img src="/images/myxine_glutinosa.png" target="_blank" width="425px" alt="woodcut sketch of myxine glutinosa, the hagfish">
  </td>
  <td style="border: 0">
    <p>The hagfish, <a href="https://en.wikipedia.org/wiki/Hagfish"><i>myxine glutinosa</i></a>, is a sea creature who hasn't changed much since well before dinosaurs roamed the planet. If it ain't broke, don't fix it! These creatures are best known for their ability to generate copious amounts of slime in short order.</p/>
    <p>By analogy, <code>myxine</code> quickly reduces the friction in creating a dynamic graphical interface, helping you to <b><i>get GUI fast</i></b> in any language under the sea.</p>
  </td>
</tr>
</table>

## TL;DR:

If you can write a function (in any language) from some internal data to a
fragment of HTML, `myxine` will give your program a **dynamic webpage** whose
content instantly reflects whatever you'd like it to show. You can listen to
events in the page to quickly prototype a user interface, with only a knowledge
of HTML and your programming language of choice.

## Getting started

### Installation

```bash
$ cargo install myxine
```

### Running

Myxine is meant to run in the background as a server process. Just run:

```bash
$ myxine
```

### Let's play!

The interface is HTTP: if you can make a web request to `localhost` from your
program, you can use `myxine`. Open your browser to `localhost:1123`, then watch
what happens when you run this command in your terminal:

```bash
$ curl "localhost:1123/swimming.html" -d "<h1>Splish splash!</h1>"
```

What's happening here?

1. If you **POST** some HTML to `localhost:1123/some/arbitrary/path`, and then
2. **GET** (i.e. navigate with your web browser) from
   `localhost:1123/some/arbitrary/path`: you'll see a web page with the HTML
   fragment you just posted set as the contents of its `<body>`.
3. When you **POST** some more HTML to that same path, the changes will be
   _instantly_ updated on the web page before your eyes!

### Setting the page title

You can also set the title of the page! Just use the `?title` query parameter,
like this:

```bash
$ curl "localhost:1123/?title=Hello%20Atlantic%20Ocean!" \
       -d "<h1>What a fine day it is!</h1>"
```

Titles will be URL-decoded, so you can use, e.g. `%20` to put a space in your title.

### Static content

You can store other kinds of data with `myxine` (such as assets you want to link
to from a dynamic page). Since `myxine`'s default behavior is to inject your
provided content into a dynamic HTML wrapper, you need to specify when you'd
like something to be hosted as a raw piece of data. This is done by appending to
your request path the query parameter `?static`. Further, if your content
is not HTML, you can change the `Content-Type` header with which it will be
served by sending a `Content-Type` header with your request.

For instance, to publish a static piece of JSON data with `curl`, you might say:

```bash
$ curl -H "Content-Type: application/json" \
       localhost:1123/swimming.json?static \
       -d '{ "splish": "splash" }'
```

You can still update the content with further `POST` requests, but a web browser
won't see those changes until someone reloads the page.

### Binary content

A common gotcha is trying to upload non-text static content (like an image) but
forgetting to send it in binary mode. This will corrupt your content in
transmission. To fix, just make sure you send the request in binary mode. For
example, to upload an image `ocean.png` with `curl`, we would say:

```bash
$ curl -H "Content-Type: image/png"      \
       "localhost:1123/ocean.png?static" \
       --data-binary @"ocean.png"
```

## Interactivity

Interfaces are meant to be *interactive*: `myxine` lets you listen to events
happening in the page without writing a lick of JavaScript.

**Step 1:** prepare a *subscription* consisting of some
[JSON](https://www.json.org/) specifying which events you care about. The format
should be a doubly-nested dictionary like this:

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

The outer dictionary maps targets in the document to event handler
specifications. Targets are named either by `id` (using a preceding `#`) or by
name (as a sequence of field selections) in the global page environment. Each
event name is mapped to a set of return values which will be sent back every
time the event occurs. These can be global properties specified like
`window.scrollY` or properties of the current event specified with a preceding
`.` like `.key`.

MDN web docs has [excellent documentation about browser
events](https://developer.mozilla.org/en-US/docs/Web/Events), which I recommend
as a reference for finding the names and properties of browser events to which
you can subscribe.

**Step 2:** Now that you have a description of the events you want to listen
for, send a **POST** request to the desired page path, with the subscription as
the body of the request and `?subscribe` as the query string. With `curl`, this
looks like:

```bash
$ curl localhost:1123/some/path?subscribe -d '{ "window": "click": [".x", ".y"] }'
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
a simple format for streams of named events with attached data. In `myxine`'s
case, every event will have an `id`, an `event`, and some attached `data`
formatted as a JSON dictionary mapping result fields you asked for to the value
they had at the time the event occurred in the page. Occasionally, the stream
will contain an empty "heartbeat" message `:`, which is used to check that
you're still listening to the stream—you can ignore these.

If your language doesn't implement a parser for this format, feel free to use
[the 17-line Python implementation](/examples/myxine.py#L14-L36) as a reference.
And please feel free to submit a pull request with your language's
implementation, or to publish it yourself! For the full technical details I used
when writing this parser, see [the W3C Recommendation for Server-Sent
Events](https://www.w3.org/TR/eventsource/) and look at the sections for
[parsing](https://www.w3.org/TR/eventsource/#parsing-an-event-stream) and
[interpretation](https://www.w3.org/TR/eventsource/#event-stream-interpretation).
You can ignore everything about what to do "as a user-agent" because you are not
a user-agent :)
