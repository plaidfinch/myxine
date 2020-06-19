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
