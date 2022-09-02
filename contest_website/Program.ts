/* eslint-disable */

import { RGBA } from "./Color";
import { Instruction } from "./Instruction";

export type ProgramMetaData = {
    width: number;
    height: number;
    backgroundColor: RGBA;
};

export class Program {
    metaData: ProgramMetaData;

    instructions: Instruction[];

    constructor(metaData: ProgramMetaData, instructions: Instruction[]) {
        this.metaData = metaData;
        this.instructions = instructions;
    }

};