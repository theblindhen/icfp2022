
/* eslint-disable */

import { BlockType, ComplexBlock, SimpleBlock } from './Block';
import { Canvas, Color } from './Canvas';
import { ColorInstruction, HorizontalCutInstruction, Instruction, InstructionType, MergeInstruction, PointCutInstruction, SwapInstruction, VerticalCutInstruction } from './Instruction';
import { InstructionCostCalculator } from './InstructionCostCalculator';
import { Parser } from './Parser';
import { Point } from './Point';
import { Program } from './Program';

export type Cost = number;

export class InterpreterResult {
    canvas: Canvas;

    cost: Cost;

    constructor(canvas: Canvas, cost: Cost) {
        this.canvas = canvas;
        this.cost = cost;
    }
}

export class Interpreter {
    topLevelIdCounter: number;

    constructor() {
        this.topLevelIdCounter = 0;
    }

    run(code: string): InterpreterResult {
        let parser = new Parser();
        let result = parser.parse(code);
        if (result.typ === 'error') {
            const [lineNumber, error] = result.result as [number, string];
            throw Error(`At ${lineNumber}, encountered: ${error}!`)
        }
        let program = result.result as Program;
        let canvas = new Canvas(
            program.metaData.width,
            program.metaData.height,
            program.metaData.backgroundColor
        );
        let totalCost = 0;
        for(let index = 0; index < program.instructions.length; index++) {
            const result = this.interpret(index, canvas, program.instructions[index]);
            canvas = result.canvas;
            totalCost += result.cost;
        }
        return new InterpreterResult(canvas, totalCost);
    }

    interpret(lineNumber: number, context: Canvas, instruction: Instruction): InterpreterResult {
        switch(instruction.typ) {
            case InstructionType.NopInstructionType || InstructionType.CommentInstructionType: {
                return new InterpreterResult(context, 0);
            }
            case InstructionType.ColorInstructionType: {
                return this.colorCanvas(lineNumber, context, instruction as ColorInstruction);
            }
            case InstructionType.PointCutInstructionType: {
                return this.pointCutCanvas(lineNumber, context, instruction as PointCutInstruction);
            }
            case InstructionType.VerticalCutInstructionType: {
                return this.verticalCutCanvas(lineNumber, context, instruction as VerticalCutInstruction);
            }
            case InstructionType.HorizontalCutInstructionType: {
                return this.horizontalCutCanvas(lineNumber, context, instruction as HorizontalCutInstruction);
            }
            case InstructionType.SwapInstructionType: {
                return this.swapCanvas(lineNumber, context, instruction as SwapInstruction);
            }
            case InstructionType.MergeInstructionType: {
                return this.mergeCanvas(lineNumber, context, instruction as MergeInstruction);
            }
        }
        throw Error(`At ${lineNumber}, encountered unreachable code!`);
    }

    colorCanvas(line: number, context: Canvas, colorInstruction: ColorInstruction): InterpreterResult {
        // TypeCheck Starts
        const {blockId, color} = colorInstruction;
        const block = context.blocks.get(blockId);
        if (!block) {
            throw Error(`At ${line}, encountered: Block Id of [${blockId}] is not found!`);
        }
        // TypeCheck Ends

        // Scoring Starts
        const cost = InstructionCostCalculator.getCost(
            InstructionType.ColorInstructionType,
            block.size.getScalarSize(),
            context.size.getScalarSize()
        )
        // Scoring Ends

        // Processing Starts
        if (block.typ === BlockType.SimpleBlockType) {
            let actualBlock = block as SimpleBlock;
            actualBlock.color = color;
            return new InterpreterResult(context, cost);
        }

        if (block.typ === BlockType.ComplexBlockType) {
            let actualBlock = block as ComplexBlock;
            context.blocks.set(
                blockId,
                new SimpleBlock(
                    actualBlock.id,
                    actualBlock.bottomLeft,
                    actualBlock.topRight,
                    color
                )
            );
            return new InterpreterResult(context, cost);
        }
        // Processing Ends
        throw Error(`At ${line}, encountered unreachable code!`);
    }

    pointCutCanvas(line: number, context: Canvas, pointCutInstruction: PointCutInstruction): InterpreterResult {
        // TypeCheck Starts
        const {blockId, point} = pointCutInstruction;
        const block = context.blocks.get(blockId);
        if (!block) {
            throw Error(`At ${line}, encountered: Block Id of [${blockId}] is not found!`);
        }
        if (!point.isStrictlyInside(block.bottomLeft, block.topRight)) {
            throw Error(`At ${line}, encountered: Point is out of the [${blockId}]! Block is from ${block.bottomLeft} to ${block.topRight}, point is at ${point}!`);
        }
        // TypeCheck Ends

        // Scoring Starts
        const cost = InstructionCostCalculator.getCost(
            InstructionType.PointCutInstructionType,
            block.size.getScalarSize(),
            context.size.getScalarSize()
        )
        // Scoring Ends

        // Processing Starts
        if (block.typ === BlockType.SimpleBlockType) {
            const bottomLeftBlock = new SimpleBlock(
                blockId + '.0',
                block.bottomLeft,
                point,
                (block as SimpleBlock).color
            );
            const bottomRightBlock = new SimpleBlock(
                blockId + '.1',
                new Point([point.px, block.bottomLeft.py]),
                new Point([block.topRight.px, point.py]),
                (block as SimpleBlock).color
            );
            const topRightBlock = new SimpleBlock(
                blockId + '.2',
                point,
                block.topRight,
                (block as SimpleBlock).color
            );
            const topLeftBlock = new SimpleBlock(
                blockId + '.3',
                new Point([block.bottomLeft.px, point.py]),
                new Point([point.px, block.topRight.py]),
                (block as SimpleBlock).color
            );
            context.blocks.delete(blockId);
            context.blocks.set(blockId + '.0', bottomLeftBlock);
            context.blocks.set(blockId + '.1', bottomRightBlock);
            context.blocks.set(blockId + '.2', topRightBlock);
            context.blocks.set(blockId + '.3', topLeftBlock);
            return new InterpreterResult(context, cost);
        }

        if (block.typ === BlockType.ComplexBlockType) {
            let bottomLeftBlocks: SimpleBlock[] = [];
            let bottomRightBlocks: SimpleBlock[] = [];
            let topRightBlocks: SimpleBlock[] = [];
            let topLeftBlocks: SimpleBlock[] = [];
            (block as ComplexBlock).subBlocks.forEach(subBlock => {
                /**
                 * __________________________
                 * |        |       |       |
                 * |   1    |   2   |   3   |
                 * |________|_______|_______|
                 * |        |       |       |
                 * |   4    |   5   |  6    |
                 * |________|_______|_______|
                 * |        |       |       |
                 * |   7    |   8   |   9   |
                 * |________|_______|_______|
                 */
                // Case 2
                if (subBlock.bottomLeft.px >= point.px && subBlock.bottomLeft.py >= point.py) {
                    topRightBlocks.push(subBlock);
                    return;
                }
                // Case 7
                if (subBlock.topRight.px <= point.px && subBlock.topRight.py <= point.py) {
                    bottomLeftBlocks.push(subBlock);
                    return;
                }
                // Case 1
                if (subBlock.topRight.px <= point.px && subBlock.bottomLeft.py >= point.py) {
                    topLeftBlocks.push(subBlock);
                    return;
                }
                // Case 9
                if (subBlock.bottomLeft.px >= point.px && subBlock.topRight.py <= point.py) {
                    bottomRightBlocks.push(subBlock);
                    return;
                }
                // Case 5
                if (point.isInside(subBlock.bottomLeft, subBlock.topRight)) {
                    bottomLeftBlocks.push(new SimpleBlock(
                        'bl_child',
                        subBlock.bottomLeft,
                        point,
                        (subBlock as SimpleBlock).color
                    ));
                    bottomRightBlocks.push(new SimpleBlock(
                        'br_child',
                        new Point([point.px, subBlock.bottomLeft.py]),
                        new Point([subBlock.topRight.px, point.py]),
                        (subBlock as SimpleBlock).color
                    ));
                    topRightBlocks.push(new SimpleBlock(
                        'tr_child',
                        point,
                        subBlock.topRight,
                        (subBlock as SimpleBlock).color
                    ));
                    topLeftBlocks.push(new SimpleBlock(
                        'tl_child',
                        new Point([subBlock.bottomLeft.px, point.py]),
                        new Point([point.px, subBlock.topRight.py]),
                        (subBlock as SimpleBlock).color
                    ));
                    return;
                }

                // Case 2
                if (subBlock.bottomLeft.px <= point.px
                    && point.px <= subBlock.topRight.px
                    && point.py < subBlock.bottomLeft.py) {
                    topLeftBlocks.push(new SimpleBlock(
                        'case2_tl_child',
                        subBlock.bottomLeft,
                        new Point([point.px, subBlock.topRight.py]),
                        (subBlock as SimpleBlock).color
                    ));
                    topRightBlocks.push(new SimpleBlock(
                        'case2_tr_child',
                        new Point([point.px, subBlock.bottomLeft.py]),
                        subBlock.topRight,
                        (subBlock as SimpleBlock).color
                    ));
                    return;
                }
                // Case 8
                if (subBlock.bottomLeft.px <= point.px
                    && point.px <= subBlock.topRight.px
                    && point.py > subBlock.topRight.py) {
                    bottomLeftBlocks.push(new SimpleBlock(
                        'case8_bl_child',
                        subBlock.bottomLeft,
                        new Point([point.px, subBlock.topRight.py]),
                        (subBlock as SimpleBlock).color
                    ));
                    bottomRightBlocks.push(new SimpleBlock(
                        'case8_br_child',
                        new Point([point.px, subBlock.bottomLeft.py]),
                        subBlock.topRight,
                        (subBlock as SimpleBlock).color
                    ));
                    return;
                }
                // Case 4
                if (subBlock.bottomLeft.py <= point.py
                    && point.py <= subBlock.topRight.py
                    && point.px < subBlock.bottomLeft.px) {
                    bottomRightBlocks.push(new SimpleBlock(
                        'case4_br_child',
                        subBlock.bottomLeft,
                        new Point([subBlock.topRight.px, point.py]),
                        (subBlock as SimpleBlock).color
                    ));
                    topRightBlocks.push(new SimpleBlock(
                        'case4_tr_child',
                        new Point([subBlock.bottomLeft.px, point.py]),
                        subBlock.topRight,
                        (subBlock as SimpleBlock).color
                    ));
                    return;
                }
                // Case 6
                if (subBlock.bottomLeft.py <= point.py
                    && point.py <= subBlock.topRight.py
                    && point.px > subBlock.topRight.px) {
                    bottomLeftBlocks.push(new SimpleBlock(
                        'case6_bl_child',
                        subBlock.bottomLeft,
                        new Point([subBlock.topRight.px, point.py]),
                        (subBlock as SimpleBlock).color
                    ));
                    topLeftBlocks.push(new SimpleBlock(
                        'case6_br_child',
                        new Point([subBlock.bottomLeft.px, point.py]),
                        subBlock.topRight,
                        (subBlock as SimpleBlock).color
                    ));
                    return;
                }
            });
            const bottomLeftBlock = new ComplexBlock(
                blockId + '.0',
                block.bottomLeft,
                point,
                bottomLeftBlocks
            );
            const bottomRightBlock = new ComplexBlock(
                blockId + '.1',
                new Point([point.px, block.bottomLeft.py]),
                new Point([block.topRight.px, point.py]),
                bottomRightBlocks
            );
            const topRightBlock = new ComplexBlock(
                blockId + '.2',
                point,
                block.topRight,
                topRightBlocks
            );
            const topLeftBlock = new ComplexBlock(
                blockId + '.3',
                new Point([block.bottomLeft.px, point.py]),
                new Point([point.px, block.topRight.py]),
                topLeftBlocks
            );
            context.blocks.delete(blockId);
            context.blocks.set(blockId + '.0', bottomLeftBlock);
            context.blocks.set(blockId + '.1', bottomRightBlock);
            context.blocks.set(blockId + '.2', topRightBlock);
            context.blocks.set(blockId + '.3', topLeftBlock);
            return new InterpreterResult(context, cost);
        }
        // Processing Ends

        throw Error(`At ${line}, encountered unreachable code!`);
    }

    verticalCutCanvas(line: number, context: Canvas, verticalCutInstruction: VerticalCutInstruction): InterpreterResult {
        // TypeCheck Starts
        const {blockId, lineNumber} = verticalCutInstruction;
        const block = context.blocks.get(blockId);
        if (!block) {
            throw Error(`At ${line}, encountered: Block Id of [${blockId}] is not found!`);
        }
        if (!(block.bottomLeft.px <= lineNumber && lineNumber <= block.topRight.px)) {
            throw Error(`At ${line}, encountered: Line number is out of the [${blockId}]! Block is from ${block.bottomLeft} to ${block.topRight}, point is at ${lineNumber}!`);
        }
        // TypeCheck Ends

        // Scoring Starts
        const cost = InstructionCostCalculator.getCost(
            InstructionType.VerticalCutInstructionType,
            block.size.getScalarSize(),
            context.size.getScalarSize()
        )
        // Scoring Ends

        // Processing Starts
        if (block.typ === BlockType.SimpleBlockType) {
            const leftBlock = new SimpleBlock(
                blockId + '.0',
                block.bottomLeft,
                new Point([lineNumber, block.topRight.py]),
                (block as SimpleBlock).color
            );
            const rightBlock = new SimpleBlock(
                blockId + '.1',
                new Point([lineNumber, block.bottomLeft.py]),
                block.topRight,
                (block as SimpleBlock).color
            );
            context.blocks.delete(blockId);
            context.blocks.set(blockId + '.0', leftBlock);
            context.blocks.set(blockId + '.1', rightBlock);
            return new InterpreterResult(context, cost);
        }

        if (block.typ === BlockType.ComplexBlockType) {
            let leftBlocks: SimpleBlock[] = [];
            let rightBlocks: SimpleBlock[] = [];
            (block as ComplexBlock).subBlocks.forEach(subBlock => {
                if (subBlock.bottomLeft.px >= lineNumber) {
                    rightBlocks.push(subBlock);
                    return;
                }
                if (subBlock.topRight.px <= lineNumber) {
                    leftBlocks.push(subBlock);
                    return;
                }
                leftBlocks.push(new SimpleBlock(
                    'child',
                    subBlock.bottomLeft,
                    new Point([lineNumber, subBlock.topRight.py]),
                    (subBlock as SimpleBlock).color
                ));
                rightBlocks.push(new SimpleBlock(
                    'child',
                    new Point([lineNumber, subBlock.bottomLeft.py]),
                    subBlock.topRight,
                    (subBlock as SimpleBlock).color
                ));
            });
            context.blocks.delete(blockId);
            const leftBlock = new ComplexBlock(
                blockId + '.0',
                block.bottomLeft,
                new Point([lineNumber, block.topRight.py]),
                leftBlocks
            );
            const rightBlock = new ComplexBlock(
                blockId + '.1',
                new Point([lineNumber, block.bottomLeft.py]),
                block.topRight,
                rightBlocks
            );
            context.blocks.set(blockId + '.0', leftBlock);
            context.blocks.set(blockId + '.1', rightBlock);
            return new InterpreterResult(context, cost);
        }
        // Processing Ends

        throw Error(`At ${line}, encountered unreachable code!`);
    }

    horizontalCutCanvas(line: number, context: Canvas, horizontalCutInstruction: HorizontalCutInstruction): InterpreterResult {
        // TypeCheck Starts
        const {blockId, lineNumber} = horizontalCutInstruction;
        const block = context.blocks.get(blockId);
        if (!block) {
            throw Error(`At ${line}, encountered: Block Id of [${blockId}] is not found!`);
        }
        if (!(block.bottomLeft.py <= lineNumber && lineNumber <= block.topRight.py)) {
            throw Error(`At ${line}, encountered: Line number is out of the [${blockId}]! Block is from ${block.bottomLeft} to ${block.topRight}, point is at ${lineNumber}!`);
        }
        // TypeCheck Ends

        // Scoring Starts
        const cost = InstructionCostCalculator.getCost(
            InstructionType.HorizontalCutInstructionType,
            block.size.getScalarSize(),
            context.size.getScalarSize()
        )
        // Scoring Ends

        // Processing Starts
        if (block.typ === BlockType.SimpleBlockType) {
            const bottomBlock = new SimpleBlock(
                blockId + '.0',
                block.bottomLeft,
                new Point([block.topRight.px, lineNumber]),
                (block as SimpleBlock).color
            );
            const topBlock = new SimpleBlock(
                blockId + '.1',
                new Point([block.bottomLeft.px, lineNumber]),
                block.topRight,
                (block as SimpleBlock).color
            );
            context.blocks.delete(blockId);
            context.blocks.set(blockId + '.0', bottomBlock);
            context.blocks.set(blockId + '.1', topBlock);
            return new InterpreterResult(context, cost);
        }

        if (block.typ === BlockType.ComplexBlockType) {
            let bottomBlocks: SimpleBlock[] = [];
            let topBlocks: SimpleBlock[] = [];
            (block as ComplexBlock).subBlocks.forEach(subBlock => {
                if (subBlock.bottomLeft.py >= lineNumber) {
                    topBlocks.push(subBlock);
                    return;
                }
                if (subBlock.topRight.py <= lineNumber) {
                    bottomBlocks.push(subBlock);
                    return;
                }
                bottomBlocks.push(new SimpleBlock(
                    'child',
                    subBlock.bottomLeft,
                    new Point([subBlock.topRight.px, lineNumber]),
                    (subBlock as SimpleBlock).color
                ));
                topBlocks.push(new SimpleBlock(
                    'child',
                    new Point([subBlock.bottomLeft.px, lineNumber]),
                    subBlock.topRight,
                    (subBlock as SimpleBlock).color
                ));
            });
            context.blocks.delete(blockId);
            const bottomBlock = new ComplexBlock(
                blockId + '.0',
                block.bottomLeft,
                new Point([block.topRight.px, lineNumber]),
                bottomBlocks
            );
            const topBlock = new ComplexBlock(
                blockId + '.1',
                new Point([block.bottomLeft.px, lineNumber]),
                block.topRight,
                topBlocks
            );
            context.blocks.set(blockId + '.0', bottomBlock);
            context.blocks.set(blockId + '.1', topBlock);
            return new InterpreterResult(context, cost);
        }
        // Processing Ends

        throw Error(`At ${line}, encountered unreachable code!`);
    }

    swapCanvas(line: number, context: Canvas, swapInstruction: SwapInstruction): InterpreterResult {
        // TypeCheck Starts
        const {blockId1, blockId2} = swapInstruction;
        const block1 = context.blocks.get(blockId1);
        if (!block1) {
            throw Error(`At ${line}, encountered: Block Id of [${blockId1}] is not found!`);
        }
        const block2 = context.blocks.get(blockId2);
        if (!block2) {
            throw Error(`At ${line}, encountered: Block Id of [${blockId2}] is not found!`);
        }
        // TypeCheck Ends

        // Scoring Starts
        const cost = InstructionCostCalculator.getCost(
            InstructionType.SwapInstructionType,
            block1.size.getScalarSize(),
            context.size.getScalarSize()
        )
        // Scoring Ends

        // Processing Starts
        if(block1.size === block2.size) {
            block1.id = blockId2;
            block2.id = blockId1;
            context.blocks.set(blockId1, block2);
            context.blocks.set(blockId2, block1);
            return new InterpreterResult(context, cost);
        } else {
            throw Error(`At ${line}, encountered: Blocks are not the same size, [${blockId1}] has size [${block1.size}] while [${blockId2}] has size [${block2.size}]`);
        }
        // Processing Ends

        throw Error(`At ${line}, encountered unreachable code!`);
    }

    mergeCanvas(line: number, context: Canvas, mergeInstruction: MergeInstruction): InterpreterResult {
        // TypeCheck Starts
        const {blockId1, blockId2} = mergeInstruction;
        const block1 = context.blocks.get(blockId1);
        if (!block1) {
            throw Error(`At ${line}, encountered: Block Id of [${blockId1}] is not found!`);
        }
        const block2 = context.blocks.get(blockId2);
        if (!block2) {
            throw Error(`At ${line}, encountered: Block Id of [${blockId2}] is not found!`);
        }
        // TypeCheck Ends

        // Scoring Starts
        const cost = InstructionCostCalculator.getCost(
            InstructionType.MergeInstructionType,
            Math.max(block1.size.getScalarSize(), block2.size.getScalarSize()),
            context.size.getScalarSize()
        )
        // Scoring Ends

        // Processing Starts
        const bottomToTop = ( block1.bottomLeft.py === block2.topRight.py ||
                            block1.topRight.py === block2.bottomLeft.py ) &&
                            block1.bottomLeft.px === block2.bottomLeft.px &&
                            block1.topRight.px === block2.topRight.px;
        if (bottomToTop) {
            this.topLevelIdCounter++;
            let newBottomLeft, newTopRight;
            if(block1.bottomLeft.py < block2.bottomLeft.py) {
                newBottomLeft = block1.bottomLeft;
                newTopRight = block2.topRight;
            } else {
                newBottomLeft = block2.bottomLeft;
                newTopRight = block1.topRight;
            }
            const newBlock = new ComplexBlock(
                this.topLevelIdCounter.toString(),
                newBottomLeft,
                newTopRight,
                block1.getChildren().concat(block2.getChildren())
            );
            context.blocks.set(newBlock.id, newBlock);
            context.blocks.delete(blockId1);
            context.blocks.delete(blockId2);
            return new InterpreterResult(context, cost);
        }

        const leftToRight = ( block1.bottomLeft.px === block2.topRight.px ||
                            block1.topRight.px === block2.bottomLeft.px ) &&
                            block1.bottomLeft.py === block2.bottomLeft.py &&
                            block1.topRight.py === block2.topRight.py
        if (leftToRight) {
            this.topLevelIdCounter++;
            let newBottomLeft, newTopRight;
            if(block1.bottomLeft.px < block2.bottomLeft.px) {
                newBottomLeft = block1.bottomLeft;
                newTopRight = block2.topRight;
            } else {
                newBottomLeft = block2.bottomLeft;
                newTopRight = block1.topRight;
            }
            const newBlock = new ComplexBlock(
                this.topLevelIdCounter.toString(),
                newBottomLeft,
                newTopRight,
                block1.getChildren().concat(block2.getChildren())
            );
            context.blocks.set(newBlock.id, newBlock);
            context.blocks.delete(blockId1);
            context.blocks.delete(blockId2);
            return new InterpreterResult(context, cost);
        }


        if(!(bottomToTop || leftToRight)) {
            throw Error(`At ${line}, encountered: Blocks [${blockId1}] and [${blockId2}] are not mergable, check canvas: [${JSON.stringify(context)}]`);
        }
        // Processing Ends

        throw Error(`At ${line}, encountered unreachable code!`);
    }

    // invert(context: Canvas, instruction: Instruction): InterpreterResult {}
}
