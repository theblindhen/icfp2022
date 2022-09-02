/* eslint-disable */

import { RGBA } from "./Color";
import { Frame } from "./Painter";

export class SimilarityChecker {
  static dataToFrame(data: [number, number, number, number][]): Frame {
    let frame: Frame = [];

    for (const item of data) {
        frame.push(new RGBA(item));
    }

    return frame;
  }

  static imageDiff(f1: Frame, f2: Frame): number {
    let diff = 0;
    let alpha = 0.005;
    for (let index = 0; index < f1.length; index++) {
        const p1 = f1[index];
        const p2 = f2[index];
        diff += this.pixelDiff(p1, p2);
    }
    return Math.round(diff * alpha);
  }

  static pixelDiff(p1: RGBA, p2: RGBA): number {
    const rDist = (p1.r - p2.r) * (p1.r - p2.r);
    const gDist = (p1.g - p2.g) * (p1.g - p2.g);
    const bDist = (p1.b - p2.b) * (p1.b - p2.b);
    const aDist = (p1.a - p2.a) * (p1.a - p2.a);
    const distance = Math.sqrt(rDist + gDist + bDist + aDist);
    return distance;
  }
}
