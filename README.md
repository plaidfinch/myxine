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

Myxine is a local web server that enables you to create interactive applications
in your web browser from the comfort of your favorite programming language. It's
designed to satisfy three explicit goals:

1. To enable programmers who don't necessarily specialize in UI design to build
   appealing interactive applications without learning a complex UI framework.
2. To make it easy to write correct, efficient, and idiomatic bindings to Myxine
   from almost any programming language.
3. To be as fast as possible while consuming as few resources as possible.

**Here's how it works:**

1. You start Myxine and open your browser to a page it is serving.
2. From your programming language of choice, you send some HTML to Myxine, and
   it instantly appears, replacing the contents of that page's `<body>`.
3. You then request a subscription to whichever browser events in which you're
   interested, and Myxine notifies you when each one occurs, eliding those you
   don't care about.
4. You can then react to those events, updating the page again to reflect what
   you'd like it to look like now. Rinse and repeat!

Myxine handles the hard work of optimizing this process to minimize latency and
computational load: it can handle thousands of requests per second and
translate them into smooth, flicker-free animations at up to 60 frames per
second.

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
maintained by the Myxine project are those for
[Python](https://pypi.org/project/myxine-client/) and
[Haskell](https://hackage.haskell.org/package/myxine-client).

If you're interested in writing Myxine bindings for a new language, you'll want
to read the [API documentation](API.md). Don't be afraid to ask for help by
[opening an issue](https://github.com/GaloisInc/myxine/issues/new), and please
do contribute back your bindings by submitting a pull request!

### An example in Python

If a picture is worth a thousand words, what's an interactive animation worth?

Below is a simple, complete Myxine application in Python. For this example to
work, you will need to install the Python client library:

``` bash
$ pip3 install myxine-client
```

To run the example, first make sure `myxine` is running on your computer:

``` bash
$ myxine
Running at: http://127.0.0.1:1123
```

Then (in another terminal window), run the script:

``` bash
$ ./examples/python/follow.py
```

Finally, navigate in your web browser to
[http://localhost:1123](http://localhost:1123), and play around! You can press
Ctrl+C in your terminal to stop the application.

You can find this example and others in the [examples](examples/) directory,
categorized in subdirectories by language of implementation.

**Without further ado, `follow.py`:**

``` python
#!/usr/bin/env python3

import random
import myxine

class Page:
    # The model of the page
    def __init__(self):
        self.x, self.y = 150, 150
        self.hue = random.uniform(0, 360)
        self.radius = 75

    # Draw the page's model as a fragment of HTML
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

    # Change the page's model in response to a browser event
    def react(self, event):
        if event.event() == 'mousemove':
            self.x = event.clientX
            self.y = event.clientY
        elif event.event() == 'mousedown':
            self.hue = (self.hue + random.uniform(30, 330)) % 360
        elif event.event() == 'wheel':
            if event.ctrlKey:
                self.hue = (self.hue + event.deltaY * -0.1) % 360
            else:
                self.radius += event.deltaY * -0.2
                self.radius = min(max(self.radius, 12), 1000)

    # The page's event loop
    def run(self, path):
        myxine.update(path, self.draw())          # Draw the page in the browser.
        try:
            for event in myxine.events(path):     # For each browser event,
                self.react(event)                 # update our model of the page,
                myxine.update(path, self.draw())  # then re-draw it in the browser.
        except KeyboardInterrupt:
            pass                                  # Press Ctrl-C to quit.

if __name__ == '__main__':
    Page().run('/')  # Run the page on the root path.
```

