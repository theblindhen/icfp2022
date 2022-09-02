/* eslint-disable */

import { Block, SimpleBlock } from './Block';
import { RGBA } from './Color';
import { Point } from './Point';

export type Color = RGBA;

export class Canvas {
    width: number;

    height: number;

    backgroundColor: Color;

    blocks: Map<string, Block>;

    constructor(width: number, height: number, backgroundColor: Color) {
        this.width = width;
        this.height = height;
        
        this.backgroundColor = backgroundColor;
        this.blocks = new Map();
        this.blocks.set(
            "0",
            new SimpleBlock(
                "0",
                new Point([0, 0]), 
                new Point([width, height]), 
                backgroundColor,
            )
        );
    }

    get size(): Point {
        return new Point([this.width, this.height]);
    }

    simplify(): SimpleBlock[] {
        let simplifiedBlocks: SimpleBlock[] = [];
        this.blocks.forEach(value => {
            simplifiedBlocks = simplifiedBlocks.concat(value.getChildren());
        });
        return simplifiedBlocks;
    }
}