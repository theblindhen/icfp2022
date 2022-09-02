/* eslint-disable */

import _ from "lodash";
import { Block } from "./Block";
import { Canvas } from "./Canvas";
import { RGBA } from "./Color";
import { Instruction, InstructionType, ColorInstruction, NopInstruction, HorizontalCutInstruction, VerticalCutInstruction, PointCutInstruction, SwapInstruction, MergeInstruction, CommentInstruction } from "./Instruction";
import { Point } from "./Point";

export class RandomInstructionGenerator {
    static generateRandomInstruction(state: Canvas): Instruction {
        let instructionType = _.sample(
            [
                InstructionType.ColorInstructionType,
                InstructionType.ColorInstructionType,
                InstructionType.ColorInstructionType,
                InstructionType.ColorInstructionType,
                InstructionType.HorizontalCutInstructionType,
                InstructionType.VerticalCutInstructionType,
                InstructionType.PointCutInstructionType,
                InstructionType.SwapInstructionType,
                InstructionType.MergeInstructionType,

            ]) as InstructionType;

        switch(instructionType) {
            case InstructionType.ColorInstructionType: {
                let block: Block = _.sample(Array.from(state.blocks.values())) as Block;
                let color = this.generateRandomColor();
                return {
                    typ: InstructionType.ColorInstructionType,
                    blockId: block.id,
                    color: color
                } as ColorInstruction;
            }
            case InstructionType.HorizontalCutInstructionType: {
                let block: Block = _.sample(Array.from(state.blocks.values())) as Block;
                let min = Math.ceil(block.bottomLeft.py) + 1;
                let max = Math.floor(block.topRight.py) - 1;
                let position = Math.floor(Math.random() * (max - min) + min);
                if (max - min <= 1) {
                    return { typ: InstructionType.NopInstructionType } as NopInstruction;
                }
                return {
                    typ: InstructionType.HorizontalCutInstructionType,
                    blockId: block.id,
                    lineNumber: position
                } as HorizontalCutInstruction;
            }
            case InstructionType.VerticalCutInstructionType: {
                let block: Block = _.sample(Array.from(state.blocks.values())) as Block;
                let min = Math.ceil(block.bottomLeft.px) + 1;
                let max = Math.floor(block.topRight.px) - 1;
                let position = Math.floor(Math.random() * (max - min) + min);
                if (max - min <= 1) {
                    return { typ: InstructionType.NopInstructionType } as NopInstruction;
                }
                return {
                    typ: InstructionType.VerticalCutInstructionType,
                    blockId: block.id,
                    lineNumber: position
                } as VerticalCutInstruction;
            }
            case InstructionType.PointCutInstructionType: {
                let block = _.sample(Array.from(state.blocks.values())) as Block;
                let xMin = block.bottomLeft.px + 1;
                let xMax = block.topRight.px - 1;
                let xPosition = Math.floor(Math.random() * (xMax - xMin) + xMin);
                let yMin = block.bottomLeft.py + 1;
                let yMax = block.topRight.py - 1;
                let yPosition = Math.floor(Math.random() * (yMax - yMin) + yMin);
                let position = new Point([xPosition, yPosition]);
                if(xMax - xMin <= 1 || yMax - yMin <= 1) {
                    return { typ: InstructionType.NopInstructionType } as NopInstruction;
                }
                return {
                    typ: InstructionType.PointCutInstructionType,
                    blockId: block.id,
                    point: position
                } as PointCutInstruction;
            }
            case InstructionType.SwapInstructionType: {
                let swapPairs: [string, string][] = []
                state.blocks.forEach((block1, blockId1) => {
                    state.blocks.forEach((block2, blockId2) => {
                        if (blockId1 != blockId2) {
                            if (block1.size === block2.size) {
                                swapPairs.push([blockId1, blockId2]);
                            }
                        }
                    })
                })
                if(swapPairs.length === 0) {
                    return { typ: InstructionType.NopInstructionType } as NopInstruction;
                }
                let pair: [string, string] = _.sample(swapPairs) as [string, string];
                return {
                    typ: InstructionType.SwapInstructionType,
                    blockId1: pair[0],
                    blockId2: pair[1]
                } as SwapInstruction;
            }
            case InstructionType.MergeInstructionType: {
                let mergePairs: [string, string][] = []
                state.blocks.forEach((block1, blockId1) => {
                    state.blocks.forEach((block2, blockId2) => {
                        if (blockId1 != blockId2) {
                            const bottomToTop = ( block1.bottomLeft.py === block2.topRight.py ||
                                block1.topRight.py === block2.bottomLeft.py ) &&
                                block1.bottomLeft.px === block2.bottomLeft.px &&
                                block1.topRight.px === block2.topRight.px;

                            const leftToRight = ( block1.bottomLeft.px === block2.topRight.px ||
                                block1.topRight.px === block2.bottomLeft.px ) &&
                                block1.bottomLeft.py === block2.bottomLeft.py &&
                                block1.topRight.py === block2.topRight.py;

                            const mergable = bottomToTop || leftToRight;
                            if (mergable) {
                                mergePairs.push([blockId1, blockId2]);
                            }
                        }
                    })
                })
                if(mergePairs.length === 0) {
                    return { typ: InstructionType.NopInstructionType } as NopInstruction;
                }
                let pair: [string, string] = _.sample(mergePairs) as [string, string];
                return {
                    typ: InstructionType.MergeInstructionType,
                    blockId1: pair[0],
                    blockId2: pair[1]
                } as MergeInstruction;
            }
        }
        return {comment: 'Should not come here'} as CommentInstruction;
    }

    static generateRandomColor(): RGBA {
        const min = Math.ceil(0);
        const max = Math.floor(255);
        const randomR = Math.floor(Math.random() * (max - min) + min);
        const randomG = Math.floor(Math.random() * (max - min) + min);
        const randomB = Math.floor(Math.random() * (max - min) + min);
        const randomA = Math.floor(Math.random() * (max - 100) + 100);
        return new RGBA([randomR, randomG, randomB, randomA]);
    }
}
