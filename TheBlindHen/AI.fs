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

let highestDistanceVerticalCut (target: ImageSlice) : int =
    [ for x in 1 .. target.size.width - 1 do
        let left_slice = subslice target { width = 1; height = target.size.height } { x = x - 1; y = 0 }
        let right_slice = subslice target { width = 1; height = target.size.height } { x = x; y = 0 }
        (x, imageDistance left_slice right_slice)
    ]
    |> List.maxBy (fun (x, d) -> d)
    |> fst

let highestDistanceHorizontalCut (target: ImageSlice) : int =
    [ for y in 1 .. target.size.height - 1 do
        let top_slice = subslice target { width = target.size.width; height = 1 } { x = 0; y = y - 1 }
        let bottom_slice = subslice target { width = target.size.width; height = 1 } { x = 0; y = y }
        (y, imageDistance top_slice bottom_slice)
    ]
    |> List.maxBy (fun (x, d) -> d)
    |> fst

let highestDistanceCut (target: ImageSlice) : Position =
    let x = highestDistanceVerticalCut target
    let y = highestDistanceHorizontalCut target
    {x=x; y=y}

let midpointCut (target: ImageSlice) : Position =
    let x = target.size.width / 2
    let y = target.size.height / 2
    {x=x; y=y}

let quadtreeSolver (splitpointSelector: ImageSlice -> Position) (target: ImageSlice) (canvas: Canvas) : ISL list =
    let topBlock = canvas.topBlocks |> Map.find "0"
    let canvasArea = topBlock.size.width * topBlock.size.height
    let rec solve (blockId: string) (targetSlice: ImageSlice) (candidateColor: Color) : ISL list * int =
        let candidateRender = renderBlock (SimpleBlock(blockId, targetSlice.size, {x = 0; y = 0}, candidateColor))
        let similarity1 = imageDistance targetSlice (sliceWholeImage candidateRender)
        let candidates =
            [
                // Option 1: leave color as is (no-op)
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
            if targetArea <= breakEvenNumberOfPixels || targetSlice.size.width <= 1 || targetSlice.size.height <= 1 then
                []
            else
            // Option 3: split the block into 4 and recurse
            let splitpoint = splitpointSelector targetSlice
            if splitpoint.x = 0 || splitpoint.x = targetSlice.size.width - 1 then
                let cost3_cut = 7 * canvasArea / targetArea
                if cost3_cut >= similarity1 then [] else
                let top_slice = subslice targetSlice { width = targetSlice.size.width; height = targetSlice.size.height - splitpoint.y } { x = 0; y = splitpoint.y }
                let bottom_slice = subslice targetSlice { width = targetSlice.size.width; height = splitpoint.y } { x = 0; y = 0 }
                let (isl3_top, cost3_top) = solve $"{blockId}.1" top_slice candidateColor
                let (isl3_bottom, cost3_bottom) = solve $"{blockId}.0" bottom_slice candidateColor
                let isl3_cut = ISL.LineCut(blockId, H, splitpoint.y + targetSlice.offset.y)
                [(isl3_cut :: isl3_top @ isl3_bottom, cost3_cut + cost3_top + cost3_bottom)]
            else
            if splitpoint.y = 0 || splitpoint.y = targetSlice.size.height - 1 then
                let cost3_cut = 7 * canvasArea / targetArea
                if cost3_cut >= similarity1 then [] else
                let left_slice = subslice targetSlice { width = splitpoint.x; height = targetSlice.size.height } { x = 0; y = 0 }
                let right_slice = subslice targetSlice { width = targetSlice.size.width - splitpoint.x; height = targetSlice.size.height } { x = splitpoint.x; y = 0 }
                let (isl3_left, cost3_left) = solve $"{blockId}.0" left_slice candidateColor
                let (isl3_right, cost3_right) = solve $"{blockId}.1" right_slice candidateColor
                let isl3_cut = ISL.LineCut(blockId, V, splitpoint.x + targetSlice.offset.x)
                [(isl3_cut :: isl3_left @ isl3_right, cost3_cut + cost3_left + cost3_right)]
            else
            let cost3_cut = 10 * canvasArea / targetArea
            if cost3_cut >= similarity1 then [] else
            let slice0 = subslice targetSlice { width = splitpoint.x; height = splitpoint.y } { x = 0; y = 0 }
            let slice1 = subslice targetSlice { width = targetSlice.size.width - splitpoint.x; height = splitpoint.y } { x = splitpoint.x; y = 0 }
            let slice2 = subslice targetSlice { width = targetSlice.size.width - splitpoint.x; height = targetSlice.size.height - splitpoint.y } { x = splitpoint.x; y = splitpoint.y }
            let slice3 = subslice targetSlice { width = splitpoint.x; height = targetSlice.size.height - splitpoint.y } { x = 0; y = splitpoint.y }
            let (isl3_0, cost3_0) = solve $"{blockId}.0" slice0 candidateColor
            let (isl3_1, cost3_1) = solve $"{blockId}.1" slice1 candidateColor
            let (isl3_2, cost3_2) = solve $"{blockId}.2" slice2 candidateColor
            let (isl3_3, cost3_3) = solve $"{blockId}.3" slice3 candidateColor
            let isl3_cut = ISL.PointCut(blockId, { x = splitpoint.x + targetSlice.offset.x; y = splitpoint.y + targetSlice.offset.y })
            [(isl3_cut :: isl3_0 @ isl3_1 @ isl3_2 @ isl3_3, cost3_cut + cost3_0 + cost3_1 + cost3_2 + cost3_3)]
        List.minBy (fun (_, penalty) -> penalty) candidates
    let res = solve "0" target {r = 255uy; g = 255uy; b = 255uy; a = 255uy}
    fst res
