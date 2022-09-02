
/* eslint-disable */

import { RGBA } from './Color';
import { Canvas } from './Canvas';
import { Point } from './Point';

export type Frame = RGBA[];

export class Painter {

    draw(canvas: Canvas): Frame {
        const blocks = canvas.simplify();
        const frame: Frame = new Array(canvas.width * canvas.height);
        let size = 0;
        blocks.forEach(block => {
            const frameTopLeft = new Point([block.bottomLeft.px, canvas.height - block.topRight.py]);
            const frameBottomRight = new Point([block.topRight.px, canvas.height - block.bottomLeft.py]);
            size += (frameBottomRight.px - frameTopLeft.px)*(frameBottomRight.py - frameTopLeft.py);
            for(let y = frameTopLeft.py ; y < frameBottomRight.py ; y++) {
                    for(let x = frameTopLeft.px; x < frameBottomRight.px ; x++) {
                    frame[y * canvas.width + x] = block.color;
                }
            }
        });
        return frame;
    }
}
