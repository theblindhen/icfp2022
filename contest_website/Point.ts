/* eslint-disable */

export class Point {
    px: number;
  
    py: number;
  
    constructor(point: [number, number] = [0, 0]) {
      [this.px, this.py] = point;
    }
  
    getPoints(): [number, number] {
      return [this.px, this.py];
    }
  
    clone() {
      return new Point(this.getPoints());
    }

    getDiff(other: Point) {
      let newX = this.px - other.px;
      let newY = this.py - other.py;

      if (newX < 0) {
        newX = 0;
      }

      if (newY < 0) {
        newY = 0;
      }

      return new Point([newX, newY]);
    }

    isStrictlyInside(bottomLeft: Point, topRight: Point) {
      return bottomLeft.px < this.px  &&
              this.px < topRight.px   &&
              bottomLeft.py < this.py &&
              this.py < topRight.py;
    }

    isOnBoundary(bottomLeft: Point, topRight: Point) {
      return (bottomLeft.px === this.px  && bottomLeft.py <= this.py && this.py <= topRight.py)
            || (topRight.px === this.px  && bottomLeft.py <= this.py && this.py <= topRight.py)
            || (bottomLeft.py === this.py  && bottomLeft.px <= this.px && this.px <= topRight.px)
            || (topRight.py === this.py  && bottomLeft.px <= this.px && this.px <= topRight.px);
    }
    
    isInside(bottomLeft: Point, topRight: Point) {
      return this.isStrictlyInside(bottomLeft, topRight) || this.isOnBoundary(bottomLeft, topRight);
    }

    getScalarSize() {
      return this.px * this.py;
    }

  }
  