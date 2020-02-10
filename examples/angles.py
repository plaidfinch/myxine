#! /usr/bin/env python3
from math import *
import myxine

class Page:
    # Keep track of where the path is
    def __init__(self, path):
        self.path = path
        self.resize()
        self.x, self.y = 0.5 * self.w, 0.5 * self.h - 0.000001
        myxine.update(self.path, self.draw())

    def update(self, event):
        if event.type == 'mousemove':
            self.x = event.x
            self.y = event.y
        elif event.type == 'resize':
            self.resize()
        myxine.update(self.path, self.draw())

    def draw(self):
        angle = degrees(atan2(self.y - self.h/2,
                              self.x - self.w/2)) + 90
        if angle < 0: angle = angle + 360
        ratio_from_edge = \
            1 - (abs(self.y - self.h/2) +
                 abs(self.x - self.w/2)) / (self.h/2 + self.w/2)
        saturation = 100 * ratio_from_edge
        lightness = 100 - 50 * ratio_from_edge

        container_style = f'''
        background: hsl({round(angle)}, {round(saturation)}%, {round(lightness)}%);
        overflow: hidden;
        margin: 0px;
        padding: 0px;
        height: 100vh;
        width: 100vw;
        text-align: center;
        position: relative;
        '''
        span_style = f'''
        transform: translate(-50%, -50%) rotate({round(angle, 2)}deg);
        position: absolute;
        top: 50%;
        font-family: Helvetica Neue;
        font-weight: 200;
        font-size: 250pt;
        color: white;
        background: rgba(0, 0, 0, 0.4);
        border-radius: {300 * ratio_from_edge}pt;
        border: none;
        padding: 100pt;
        width: 550pt;
        text-shadow: 0 0 25pt black;
        '''
        html = f'''
        <div id="container" style="{container_style}">
            <span style="{span_style}">
                {round(angle)}°
            </span>
        </div>'''
        return html

    def resize(self):
        self.w, self.h = \
            myxine.evaluate(self.path, expression='[window.innerWidth, window.innerHeight]')

    def run(self):
        for event in myxine.subscribe(self.path):
            self.update(event)

def main():
    try:
        path = '/'
        print('Running at:', myxine.page_url(path))
        Page(path).run()
    except KeyboardInterrupt: pass

if __name__ == '__main__': main()
