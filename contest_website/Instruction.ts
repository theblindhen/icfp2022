
/* eslint-disable */

import { RGBA } from './Color';
import { Point } from './Point';

export type LineNumber = number;

export enum InstructionType {
    NopInstructionType = 'NOP',
    CommentInstructionType = 'COMMENT',
    ColorInstructionType = 'COLOR',
    PointCutInstructionType = 'POINT_CUT',
    VerticalCutInstructionType = 'VERTICAL_CUT',
    HorizontalCutInstructionType = 'HORIZONTAL_CUT',
    SwapInstructionType = 'SWAP',
    MergeInstructionType = 'MERGE',
};

export type NopInstruction = {
    typ: InstructionType.NopInstructionType,
};

export type CommentInstruction = {
    typ: InstructionType.CommentInstructionType,
    comment: string,
}

export type ColorInstruction = {
    typ: InstructionType.ColorInstructionType,
    blockId: string,
    color: RGBA
};

export type PointCutInstruction = {
    typ: InstructionType.PointCutInstructionType,
    blockId: string,
    point: Point
};

export type VerticalCutInstruction = {
    typ: InstructionType.VerticalCutInstructionType,
    blockId: string,
    lineNumber: LineNumber,
};

export type HorizontalCutInstruction = {
    typ: InstructionType.HorizontalCutInstructionType,
    blockId: string,
    lineNumber: LineNumber,
};

export type SwapInstruction = {
    typ: InstructionType.SwapInstructionType,
    blockId1: string,
    blockId2: string,
};

export type MergeInstruction = {
    typ: InstructionType.MergeInstructionType,
    blockId1: string,
    blockId2: string,
};

export type Instruction = 
    | NopInstruction
    | CommentInstruction
    | ColorInstruction
    | PointCutInstruction
    | VerticalCutInstruction
    | HorizontalCutInstruction
    | SwapInstruction
    | MergeInstruction;

export const instructionToString = (instruction: Instruction) => {
    switch (instruction.typ) {
        case InstructionType.NopInstructionType: {
            const instructionAsString = "";
            return instructionAsString;
        }
        case InstructionType.CommentInstructionType: {
            const instructionAsString = `# ${instruction.comment}`;
            return instructionAsString;
        }
        case InstructionType.ColorInstructionType: {
            const instructionAsString = `color [${instruction.blockId}] [${instruction.color.r}, ${instruction.color.g}, ${instruction.color.b}, ${instruction.color.a}]`;
            return instructionAsString;
        }
        case InstructionType.PointCutInstructionType: {
            const instructionAsString = `cut [${instruction.blockId}] [${instruction.point.px}, ${instruction.point.py}]`;
            return instructionAsString;
        }
        case InstructionType.VerticalCutInstructionType: {
            const instructionAsString = `cut [${instruction.blockId}] [x] [${instruction.lineNumber}]`;
            return instructionAsString;
        }
        case InstructionType.HorizontalCutInstructionType: {
            const instructionAsString = `cut [${instruction.blockId}] [y] [${instruction.lineNumber}]`;
            return instructionAsString;
        }
        case InstructionType.SwapInstructionType: {
            const instructionAsString = `swap [${instruction.blockId1}] [${instruction.blockId2}]`;
            return instructionAsString;
        }
        case InstructionType.MergeInstructionType: {
            const instructionAsString = `merge [${instruction.blockId1}] [${instruction.blockId2}]`;
            return instructionAsString;
        }
    }
}
