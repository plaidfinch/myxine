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

If you can write a function (in any language) from some internal data to a fragment of HTML, `myxine` will give your program a **dynamic webpage** whose content instantly reflects whatever your imagination wills it to show.

The interface is HTTP: if you can make a web request to `localhost` from your program, you can use `myxine`:

```
$ curl localhost:7245/swimming.html -d "<h1>Splish splash!</h1>"
```

1. **POST** some HTML to `localhost:7245/some/arbitrary/path`, and then
2. **GET** (i.e. navigate with your web browser) to `localhost:2628/some/arbitrary/path`: you'll see a web page with the HTML you just posted as its body.
3. If you **POST** some more HTML to that same path, the changes will be _instantly_ updated on the web page.

### Static content

You can store other kinds of data with `myxine`, too, but it won't be served dynamically. Just set the `Content-Type` header on your `POST` request to something specific like `application/json`. With `curl`, this amounts to something like:

```
$ curl -H "Content-Type: application/json" localhost:7245/swimming.json -d '{ "splish": "splash" }'
```

You can still update the content with further `POST` requests, but a web browser won't see those changes until someone reloads the page.

### Static _binary_ content

A common gotcha is trying to upload non-text static content (like an image) but forgetting to send it in binary mode. This will corrupt your content in transmission. To fix, just make sure you send the request in binary mode. For example, with `curl`, we would say:

```
$ curl -H "Content-Type: image/png" "http://localhost:7245/eel.png" --data-binary @"eel.png"
```

## FAQ

**Q:** Why these particular port numbers? <br>
**A:** On a telephone keypad, `7245` spells `S`-`A`-`I`-`L`, and `2628` spells `B`-`O`-`A`-`T`.
