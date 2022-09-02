module AI

open Model
open Util
open Instructions

/// The number of pixels that should never be sub-divided further because
/// painting them would cost too much compared to the improved similarity.
let breakEvenNumberOfPixels = 194

/// Return the instruction which colors the input block according to the median
/// color of the target image. Assume the target image slice conforms to the 
let colorBlockMedian (target: ImageSlice) (block: Block) : ISL =
    let color = medianColor target
    ISL.ColorBlock(block.id, color)

let quadtreeSolver (target: ImageSlice) (canvas: Canvas) : ISL list =
    let topBlock = canvas.topBlocks |> Map.find "0"
    let canvasArea = topBlock.size.width * topBlock.size.height
    let rec solve (blockId: string) (targetSlice: ImageSlice) (candidateColor: Color) : ISL list * int =
        let candidates =
            [
                // Option 1: leave color as is (no-op)
                let candidateRender = renderBlock (SimpleBlock(blockId, targetSlice.size, {x = 0; y = 0}, candidateColor))
                let similarity1 = imageDistance targetSlice (sliceWholeImage candidateRender)
                ([], similarity1)
            ] @ (
                // Option 2: paint the whole block with the median color
                let medianColor = averageColor targetSlice
                if medianColor = candidateColor then [] else
                let isl2_color = ISL.ColorBlock(blockId, medianColor)
                let cost2_color = 5 * canvasArea / (targetSlice.size.width * targetSlice.size.height)
                let isl2, cost2 = solve blockId targetSlice medianColor
                [(isl2_color :: isl2, cost2_color + cost2)]
            ) @
            let targetArea = targetSlice.size.width * targetSlice.size.height
            if targetArea <= breakEvenNumberOfPixels then
                []
            else
            // Option 3: split the block into 4 and recurse
            let midpoint = { x = targetSlice.size.width / 2; y = targetSlice.size.height / 2 }
            let slice0 = subslice targetSlice { width = midpoint.x; height = midpoint.y } { x = 0; y = 0 }
            let slice1 = subslice targetSlice { width = targetSlice.size.width - midpoint.x; height = midpoint.y } { x = midpoint.x; y = 0 }
            let slice2 = subslice targetSlice { width = targetSlice.size.width - midpoint.x; height = targetSlice.size.height - midpoint.y } { x = midpoint.x; y = midpoint.y }
            let slice3 = subslice targetSlice { width = midpoint.x; height = targetSlice.size.height - midpoint.y } { x = 0; y = midpoint.y }
            let (isl3_0, cost3_0) = solve $"{blockId}.0" slice0 candidateColor
            let (isl3_1, cost3_1) = solve $"{blockId}.1" slice1 candidateColor
            let (isl3_2, cost3_2) = solve $"{blockId}.2" slice2 candidateColor
            let (isl3_3, cost3_3) = solve $"{blockId}.3" slice3 candidateColor
            let isl3_cut = ISL.PointCut(blockId, { x = midpoint.x + targetSlice.offset.x; y = midpoint.y + targetSlice.offset.y })
            let cost3_cut = 10 * canvasArea / targetArea
            [(isl3_cut :: isl3_0 @ isl3_1 @ isl3_2 @ isl3_3, cost3_cut + cost3_0 + cost3_1 + cost3_2 + cost3_3)]
        List.minBy (fun (_, penalty) -> penalty) candidates
    solve "0" target {r = 255uy; g = 255uy; b = 255uy; a = 255uy} |> fst
