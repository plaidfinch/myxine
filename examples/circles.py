#! /usr/bin/env python3
from math import *
from uuid import *
import random
import myxine

class Circle:
    current_z_index = 0  # increasing counter for z-index of circles

    def __init__(self, *, x, y, r):
        self.hue = round(random.uniform(0, 360)) # random hue
        self.x = x  # x-coordinate for origin
        self.y = y  # y-coordinate for origin
        self.r = r  # radius of circle
        self.z = Circle.current_z_index # put it on top of all the others
        Circle.current_z_index += 1

    def draw(self, current=False):
        border_width = 2;
        return f'''<div style="position: absolute;
                               top: {round(self.y - self.r/2 - border_width/2)}px;
                               left: {round(self.x - self.r/2 - border_width/2)}px;
                               width: {round(self.r)}px;
                               height: {round(self.r)}px;
                               z-index: {self.z};
                               background: hsla({self.hue}, 100%, 75%, 25%);
                               border: {border_width}px solid hsla({self.hue}, 50%, 50%, 75%);
                               border-radius: {round(self.r)}px;"></div>'''

class State:
    current = None   # The currently-in-progress circle, if any
    rest = []        # The already-drawn circles
    (x, y) = (0, 0)  # The current mouse location

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
        for circle in self.rest:
            circles.append(circle.draw())
        if self.current is not None:
            circles.append(self.current.draw(current=True))
        if circles != []:
            content = ''.join(circles)
        else:
            content = '''<span style="transform: translate(-50%, -100%);
                                      text-align: center; width: 100vw;
                                      position: absolute;
                                      top: 50%; left: 50%;
                                      font-family: Helvetica Neue;
                                      font-size: 50pt; color: darkgrey">
                            Click & drag to make art!
                         </span>'''
        return f'''<div style="position: relative; padding: 0px;
                               height: 100vh; width: 100vw;
                               overflow: hidden;">{content}</div>'''

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
