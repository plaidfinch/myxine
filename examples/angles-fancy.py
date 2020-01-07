from dataclasses import *
from math import *

import myxine

class State:
    # The dimensions of the browser window
    (w, h) = (1, 1)
    # The location of the cursor relative to the browser window
    (x, y) = (0.5, 0.5 - 0.000001)

    def set_window(self, *, w: int, h: int) -> bool:
        self.w = w
        self.h = h
        return True # could instead look for changes

    def set_mouse(self, *, x: int, y: int) -> bool:
        self.x = x
        self.y = y
        return True # could instead look for changes

    def set_all(self, *, x: int, y: int, w: int, h: int) -> bool:
        return self.set_mouse(x=x, y=y) or self.set_window(w=w, h=h)

def draw(state):
    angle = round(degrees(atan2(state.y - state.h/2,
                                state.x - state.w/2)) + 90)
    if angle < 0: angle = angle + 360
    ratio_from_edge = \
        1 - (abs(state.y - state.h/2) +
             abs(state.x - state.w/2)) / (state.h/2 + state.w/2)
    saturation = 100 * ratio_from_edge
    lightness = 100 - 50 * ratio_from_edge

    container_style = """
    overflow: hidden;
    margin: 0px;
    padding: 0px;
    height: 100vh;
    width: 100vw;
    background: hsl({angle}, {saturation}%, {lightness}%);
    text-align: center;
    position: relative;
    """.format(angle = angle,
                saturation = saturation,
                lightness = lightness)
    span_style = """
    position: absolute;
    top: 50%;
    transform: translate(-50%, -50%) rotate({angle}deg);
    font-family: Helvetica Neue;
    font-weight: 200;
    font-size: 250pt;
    color: white;
    background: rgba(0, 0, 0, 0.4);
    border-radius: 300pt;
    border: none;
    padding: 100pt;
    width: 550pt;
    text-shadow: 0 0 25pt black;
    """.format(angle = angle)
    html = "                                           \
    <div id=\"container\" style=\"{container_style}\"> \
        <span style=\"{span_style}\">                  \
            {angle}°                                   \
        </span>                                        \
    </div>".format(container_style = container_style,
                    span_style = span_style,
                    angle = angle)
    return (None, html)

def main():
    page = myxine.Page(state = State())
    page.on('mousemove', '#container',
            ['.clientX', '.clientY', 'window.innerHeight', 'window.innerWidth'],
            (lambda s, d: s.set_all(w=d[0], h=d[1], x=d[2], y=d[3])))
    page.on('resize', 'window',
            ['window.innerWidth', 'window.innerHeight'],
            (lambda s, d: s.set_window(w=d[0], h=d[1])))
    page.render_with(draw)
    page.start()
    page.join()

if __name__ == '__main__': main()
