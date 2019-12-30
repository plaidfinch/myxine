# `myxine`: a slithery sea-friend to help you _get GUI fast_

<table style="border: 0">
<tr style="border: 0">
  <td width="425px" style="border: 0">
    <img src="/images/myxine_glutinosa.png" target="_blank" width="425px" alt="woodcut sketch of myxine glutinosa, the hagfish">
  </td>
  <td style="border: 0">
    <p>The hagfish, <a href="https://en.wikipedia.org/wiki/Hagfish"><i>myxine glutinosa</i></a>, is a sea creature who hasn't changed much since well before dinosaurs roamed the planet. If it ain't broke, don't fix it! These creatures are best known for their ability to generate copious amounts of slime in short order.</p/>
    <p>By analogy, <code>myxine</code> quickly reduces the friction in creating a dynamic graphical interface, helping you to <b><i>get GUI fast</i></b>.</p>
  </td>
</tr>
</table>

## TL;DR:

If you can write a function (in any language) from some internal data to a
fragment of HTML, `myxine` will give your program a **dynamic webpage** whose
content instantly reflects whatever your imagination wills it to show. Just run
in the background:

```
$ myxine --port 1123 &
```

The interface is HTTP: if you can make a web request to `localhost` from your
program, you can use `myxine`:

```
$ curl localhost:1123/swimming.html -d "<h1>Splish splash!</h1>"
```

1. **POST** some HTML to `localhost:1123/some/arbitrary/path`, and then
2. **GET** (i.e. navigate with your web browser) to
   `localhost:1123/some/arbitrary/path`: you'll see a web page with the HTML
   fragment you just posted set as its body.
3. If you **POST** some more HTML to that same path, the changes will be
   _instantly_ updated on the web page before your eyes!

## Setting the page title

You can also set the title of the page! Just use the `?title` query parameter, like this:

```
$ curl localhost:1123/?title=Hello%20Atlantic%20Ocean! -d "<h1>What a fine day it is!</h1>"
```

Titles will be URL-decoded, so you can use, e.g. `%20` to put a space in your title.

## Static content

You can store other kinds of data with `myxine` (such as assets you want to link
to from a dynamic page). Since `myxine`'s default behavior is to inject your
provided content into a dynamic HTML wrapper, you need to specify when you'd
like something to be hosted as a raw piece of data. This is done by appending to
your request path the query parameter `?static=true`. Further, if your content
is not HTML, you can change the `Content-Type` header with which it will be
served by sending a `Content-Type` header with your request.

For instance, to publish a static piece of JSON data with `curl`, you might say:

```
$ curl -H "Content-Type: application/json" localhost:1123/swimming.json?static=true -d '{ "splish": "splash" }'
```

You can still update the content with further `POST` requests, but a web browser
won't see those changes until someone reloads the page.

### Static _binary_ content

A common gotcha is trying to upload non-text static content (like an image) but
forgetting to send it in binary mode. This will corrupt your content in
transmission. To fix, just make sure you send the request in binary mode. For
example, with `curl`, we would say:

```
$ curl -H "Content-Type: image/png" "localhost:1123/eel.png?static=true" --data-binary @"eel.png"
```
