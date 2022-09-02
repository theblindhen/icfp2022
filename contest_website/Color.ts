/* eslint-disable */

export class RGBA {
    r: number;
  
    g: number;
    
    b: number;
  
    a: number;
  
    constructor(rgba: [number, number, number, number] = [0, 0, 0, 0]) {
      [this.r, this.g, this.b, this.a] = rgba;
    }
  }

  