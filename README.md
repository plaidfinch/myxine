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

Myxine is a local web server that provides an API for creating interactive
applications in your web browser, from the comfort of your favorite programming
language and its HTTP library.

TODO: explain Myxine's model

## Installing

Myxine itself is a server that you run locally and connect to from a client
library in some programming language.

To install the Myxine server, you will need a recent version of the Rust
programming langauge and its build tool, `cargo`. If you don't have that,
[here's the quick-start for installing
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

## Building interactive applications

To build an interactive application with Myxine (or add a new interface to an
existing application!), you will likely want to use one of the client libraries
in your language of choice. Currently, the two client libraries officially
maintained by the Myxine project are those for Python and Haskell.

However, if your favorite language is not supported, don't despair: one of
Myxine's explicit design goals is to make it easy to write succinct, idiomatic
bindings in almost any programming language. If you're interested in writing
Myxine bindings for a new language, you'll want to read the [API
documentation](API.md). Don't be afraid to ask for help by [opening an
issue](https://github.com/GaloisInc/myxine/issues/new), and please do contribute
back your bindings by submitting a pull request!

If a client library already exists for the language you're using, you'll want to
read the documentation for that specific library to find out how to use it.
Typically, a client library does not manage the `myxine` server process itself;
rather, it expects you to make sure it's running (either in your application
code, or as a user).

### An example

Without further ado, here's a simple, complete Myxine application written in
Python. To run it, make sure `myxine` is running on your computer, then run the
script and navigate in your web browser to [http://localhost:1123].

Try moving the mouse, clicking, and scrolling (with and without modifier keys
held down), and see what happens!

``` python
#!/usr/bin/env python3

import random
import myxine

def draw(x, y, hue, radius):
    radius = round(radius)
    style = f'''
       position: absolute;
       height: {radius*2}px;
       width: {radius*2}px;
       top: {y}px;
       left: {x}px;
       transform: translate(-50%, -50%);
       border-radius: 50%;
       border: {round(radius/2)}px solid hsl({hue}, 80%, 70%);
       background: hsl({hue+120}, 80%, 60%)
    '''
    background_style = f'''
        position: absolute;
        overflow: hidden;
        width: 100vw;
        height: 100vh;
        background: hsl({hue-120}, 80%, 80%);
    '''
    return f'''
        <div style="{background_style}">
            <div style="{style}"></div>
        </div>
    '''

def main():
    try:
        # parameters of the model
        x, y = 0, 0
        hue = random.uniform(0, 360)
        radius = 75

        path = '/'  # serve the page on this path

        # draw the initial page
        myxine.update(path, draw(x, y, hue, radius))

        # loop over events in the page
        for event in myxine.events(path):
            if event.type == 'mousemove':
                x = event.clientX
                y = event.clientY
            elif event.type == 'click':
                hue = hue + random.uniform(30, 330) % 360
            elif event.type == 'wheel':
                if event.ctrlKey or event.metaKey or event.altKey:
                    hue += event.deltaY * -0.1
                else:
                    radius += event.deltaY * -0.2
                    radius = min(max(radius, 12), 1000)
            myxine.update(path, draw(x, y, hue, radius))

    except KeyboardInterrupt:
        pass

if __name__ == '__main__':
    main()
```

This example demonstrates that a straightforward minimal Myxine application has
very little input lag, despite the simplicity of Myxine's design---try moving
your mouse as fast as possible, and you'll find that it's quite difficult to
outrun the circle by very much.

You can find this example and others in the [examples](examples/) directory,
categorized in subdirectories by language of implementation.

