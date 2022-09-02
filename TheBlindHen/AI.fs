module AI

open Model
open Util
open Instructions

/// Return the instruction which colors the input block according to the median
/// color of the target image. Assume the target image slice conforms to the 
let color_block_median (target: ImageSlice) (block: Block) : ISL =
    let color = median_color target
    ISL.ColorBlock(block.id, color)