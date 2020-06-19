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
Python. For this example to work, you will need to install the Python client
library:

``` bash
$ pip3 install myxine-client
```

To run the example, first make sure `myxine` is running on your computer:

``` bash
$ myxine
```

Then, run the script:

``` bash
$ ./examples/python/follow.py
```

Finally, navigate in your web browser to
[http://localhost:1123](http://localhost:1123), and play around!

``` python
#!/usr/bin/env python3

import random
import myxine

class Page:
    def __init__(self):
        self.x, self.y = 0, 0
        self.hue = random.uniform(0, 360)
        self.radius = 75

    def react(self, event):
        if event.type == 'mousemove':
            self.x = event.clientX
            self.y = event.clientY
        elif event.type == 'mousedown':
            self.hue = (self.hue + random.uniform(30, 330)) % 360
        elif event.type == 'wheel':
            if event.ctrlKey:
                self.hue = (self.hue + event.deltaY * -0.1) % 360
            else:
                self.radius += event.deltaY * -0.2
                self.radius = min(max(self.radius, 12), 1000)

    def draw(self):
        circle_style = f'''
        position: absolute;
        height: {round(self.radius*2)}px;
        width: {round(self.radius*2)}px;
        top: {self.y}px;
        left: {self.x}px;
        transform: translate(-50%, -50%);
        border-radius: 50%;
        border: {round(self.radius/2)}px solid hsl({round(self.hue)}, 80%, 80%);
        background: hsl({round(self.hue+120)}, 80%, 75%)
        '''
        background_style = f'''
        position: absolute;
        overflow: hidden;
        width: 100vw;
        height: 100vh;
        background: hsl({round(self.hue-120)}, 80%, 90%);
        '''
        instructions_style = f'''
        position: absolute;
        bottom: 30px;
        left: 30px;
        font-family: sans-serif;
        font-size: 22pt;
        user-select: none;
        '''
        return f'''
        <div style="{background_style}">
            <div style="{instructions_style}">
                <b>Move the mouse</b> to move the circle<br/>
                <b>Scroll</b> to change the circle's size<br/>
                <b>Ctrl + Scroll</b> to change the color scheme<br/>
                <b>Click</b> to randomize the color scheme<br/>
            </div>
            <div style="{circle_style}"></div>
        </div>
        '''

def run(path):
    page = Page()                             # Create our model of the page,
    myxine.update(path, page.draw())          # then draw it in the browser.
    try:
        for event in myxine.events(path):     # For each browser event,
            page.react(event)                 # update our model of the page,
            myxine.update(path, page.draw())  # then re-draw it in the browser.
    except KeyboardInterrupt:
        pass                                  # Press Ctrl-C to quit.

if __name__ == '__main__':
    run('/')  # Run the page on the root path.
```

This example demonstrates that a straightforward minimal Myxine application has
very little input lag, despite the simplicity of Myxine's design---try moving
your mouse as fast as possible, and you'll find that it's quite difficult to
outrun the circle by very much.

You can find this example and others in the [examples](examples/) directory,
categorized in subdirectories by language of implementation.

