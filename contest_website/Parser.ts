/* eslint-disable */

import { Program, ProgramMetaData } from './Program';
import { 
  Instruction, 
  InstructionType,
  NopInstruction,
  CommentInstruction,
  ColorInstruction,
  PointCutInstruction,
  VerticalCutInstruction,
  HorizontalCutInstruction,
  SwapInstruction,
  MergeInstruction,
} from './Instruction';
import { RGBA } from './Color';
import { Point } from './Point';

export type ParseError = [number, string];
export type ParseResult = {
  typ: string,
  result: Program | Instruction | ParseError,
};

export class Parser {

  parse(code: string): ParseResult {
    const instructions: Array<Instruction> = [];
    // TODO: needs line trimming (\r or others)
    const splitProgram : Array<[number, string]> = [];
    code.split('\n').forEach((value, index) => { splitProgram.push([index, value])});
    for (const [index, line] of splitProgram) {
      const parseResult = this.parseLine(index, line);
      if (parseResult.typ === 'error') {
        return parseResult;
      }
      const instruction = parseResult.result as Instruction;
      if (!(instruction.typ === InstructionType.NopInstructionType 
          || instruction.typ === InstructionType.CommentInstructionType)) {
        instructions.push(instruction);
      }
    }
    let metaData = { width: 400, height: 400, backgroundColor: new RGBA([255, 255, 255, 255]) };

    return { typ: 'program', result: new Program(metaData, instructions) };
}



  parseLine(lineNumber: number, line: string): ParseResult {
    if(line.trim() === '') {
        let result = { typ: InstructionType.NopInstructionType} as NopInstruction
        return { typ: 'instruction', result: result};
    }
    if(line.startsWith('#')) {
        let result = { typ: InstructionType.CommentInstructionType, comment: line.substring(1).trim()} as CommentInstruction;
        return { typ: 'instruction', result: result};
    }
    line = line.replace(/\s+/g, '');
    const number_re = `\(0|\[1-9\]\[0-9\]*\)`;
    const byte_re = `\(25\[0-5\]|2\[0-4\]\[0-9\]|1\[0-9\]\[0-9\]|\[1-9\]\[0-9\]|\[0-9\]\)`;
    const color_re = `${byte_re},${byte_re},${byte_re},${byte_re}`;
    const block_id_re = `${number_re}\(\\\.${number_re}\)*`;
    const point_re = `${number_re},${number_re}`;
    const COLOR_INSTRUCTION_REGEX = 
        new RegExp(`color\\\[\(${block_id_re}\)\\\]\\\[(${color_re})\\\]$`);
    const LINE_CUT_INSTRUCTION_REGEX = 
        new RegExp(`cut\\\[\(${block_id_re}\)\\\]\\\[\(x|X|y|Y\)\\\]\\\[\(${number_re}\)\\\]$`);
    const POINT_CUT_INSTRUCTION_REGEX = 
        new RegExp(`cut\\\[\(${block_id_re}\)\\\]\\\[\(${point_re}\)\\\]$`);
    const MERGE_INSTRUCTION_REGEX =
        new RegExp(`merge\\\[\(${block_id_re}\)\\\]\\\[\(${block_id_re}\)\\\]$`);
    const SWAP_INSTRUCTION_REGEX =
        new RegExp(`swap\\\[\(${block_id_re}\)\\\]\\\[\(${block_id_re}\)\\\]$`);

    const colorMatchResult = line.match(COLOR_INSTRUCTION_REGEX);
    if (colorMatchResult) {
      const typ = InstructionType.ColorInstructionType;
      const blockId = colorMatchResult[1];
      const color = new RGBA(colorMatchResult.slice(6,).map(value => +value) as [number, number, number, number])
      return { typ: 'instruction', result: { typ, blockId, color } as ColorInstruction };
    }

    const lineCutMatchResult = line.match(LINE_CUT_INSTRUCTION_REGEX);
    if (lineCutMatchResult) {
      const blockId = lineCutMatchResult[1];
      const orientation = lineCutMatchResult[5];
      const lineNumber = +lineCutMatchResult[6];
      if (orientation === 'x' || orientation === 'X') {
        const typ = InstructionType.VerticalCutInstructionType;
        return { typ: 'instruction', result: { typ, blockId, lineNumber } as VerticalCutInstruction };
      } else {
        const typ = InstructionType.HorizontalCutInstructionType;
        return { typ: 'instruction', result: { typ, blockId, lineNumber } as HorizontalCutInstruction };
      }
    }

    const pointCutMatchResult = line.match(POINT_CUT_INSTRUCTION_REGEX);
    if (pointCutMatchResult) {
      const typ = InstructionType.PointCutInstructionType;
      const blockId = pointCutMatchResult[1];
      const px = +pointCutMatchResult[6];
      const py = +pointCutMatchResult[7];
      const point = new Point([px, py]);
      return { typ: 'instruction', result: { typ, blockId, point } as PointCutInstruction };
    }     

    const mergeMatchResult = line.match(MERGE_INSTRUCTION_REGEX);
    if (mergeMatchResult) {
      const typ = InstructionType.MergeInstructionType;
      const blockId1 = mergeMatchResult[1];
      const blockId2 = mergeMatchResult[5];
      return { typ: 'instruction', result: { typ, blockId1, blockId2 } as MergeInstruction };
    }

    const swapMatchResult = line.match(SWAP_INSTRUCTION_REGEX);
    if (swapMatchResult) {
      const typ = InstructionType.SwapInstructionType;
      const blockId1 = swapMatchResult[1];
      const blockId2 = swapMatchResult[5];
      return { typ: 'instruction', result: { typ, blockId1, blockId2 } as SwapInstruction };
    }

    return { typ: 'error', result: [lineNumber, `Cannot parse the instruction [${line}]!`] };
  }

}