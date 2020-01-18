#! /usr/bin/env python3
from math import *
from uuid import *
import random
import myxine

current_z_index = 0

class Circle:
    # Parameters (pixels)
    x: float  # x-position
    y: float  # y-position
    r: float  # radius

    def __init__(self, *, x, y, r):
        self.hue = random.uniform(0, 360)
        self.x = x
        self.y = y
        self.r = r
        global current_z_index
        self.z = current_z_index
        current_z_index += 1

    def draw(self, current=False):
        border_width = 2;
        return f'''<div style="position: absolute;
                               top: {self.y - self.r/2 - border_width/2}px;
                               left: {self.x - self.r/2 - border_width/2}px;
                               width: {self.r}px;
                               height: {self.r}px;
                               z-index: {self.z};
                               background: hsla({self.hue}, 100%, 75%, 25%);
                               border: {border_width}px solid hsla({self.hue}, 50%, 50%, 75%);
                               border-radius: {self.r}px;"></div>'''

class State:
    # The currently-in-progress circle, if any
    current = None
    # The already-drawn circles
    rest = []
    # The current mouse location
    (x, y) = (0, 0)

    def update(self, e):
        if e.event() == 'mousedown':
            self.current = Circle(x = self.x, y = self.y, r = 0)
        elif e.event() == 'mouseup':
            if self.current is not None:
                self.rest.append(self.current)
                self.current = None
        elif e.event() == 'mousemove':
            self.x = e['.x']
            self.y = e['.y']
            if self.current is not None:
                self.current.r = 2* sqrt((self.x - self.current.x)**2 +
                                         (self.y - self.current.y)**2)

    def draw(self):
        circles = []
        if self.current is not None:
            circles.append(self.current.draw(current=True))
        for circle in self.rest:
            circles.append(circle.draw())
        return f'''<div id="container"
                        style="position: relative;
                               height: 100vh;
                               width: 100vw;
                               overflow: hidden;">{''.join(circles)}</div>'''

# A description of the events we wish to monitor
subscription = {
    'window': {
        'mousemove': ['.x', '.y'],
        'mouseup': [],
        'mousedown': [],
    },
}

def main():
    try:
        path = '/'
        print('Running at:', myxine.page_url(path))

        # Make a new state object
        state = State()

        # Draw the page for the first time
        myxine.update(path, state.draw())

        # Iterate over all page events, updating the page each time
        for event in myxine.subscribe(path, subscription):
            state.update(event)
            myxine.update(path, state.draw())

    # You can kill the program with a keyboard interrupt
    except KeyboardInterrupt: pass
    except Exception as e: print('Exception: ', e)

if __name__ == '__main__': main()
