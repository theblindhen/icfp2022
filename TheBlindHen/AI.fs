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

let highestDistanceVerticalCut (target: ImageSlice) : int option =
    let minWidth = 1
    let candidate =
        (None, 0)::
        [ for x in minWidth .. target.size.width - minWidth do
            let left_slice = subslice target { width = 1; height = target.size.height } { x = x - 1; y = 0 }
            let right_slice = subslice target { width = 1; height = target.size.height } { x = x; y = 0 }
            (Some x, imageSimilarity left_slice right_slice)
        ]
        |> List.maxBy (fun (x, d) -> d)
        |> fst
    match candidate with
    | Some x when x = minWidth || x = target.size.width - minWidth -> None
    | x -> x

let highestDistanceHorizontalCut (target: ImageSlice) : int option =
    let minHeight = 1
    let candidate =
        (None, 0)::
        [ for y in minHeight .. target.size.height - minHeight do
            let top_slice = subslice target { width = target.size.width; height = 1 } { x = 0; y = y - 1 }
            let bottom_slice = subslice target { width = target.size.width; height = 1 } { x = 0; y = y }
            (Some y, imageSimilarity top_slice bottom_slice)
        ]
        |> List.maxBy (fun (x, d) -> d)
        |> fst
    match candidate with
    | Some y when y = minHeight || y = target.size.height - minHeight -> None
    | y -> y

let highestDistanceCut (target: ImageSlice) : int option * int option =
    (highestDistanceVerticalCut target, highestDistanceHorizontalCut target)

let midpointCut (target: ImageSlice) : int option * int option =
    let x = target.size.width / 2
    let y = target.size.height / 2
    (Some x, Some y)

/// Returns instructions, cost, and similarity (scaled)
let quadtreeSolver (splitpointSelector: ImageSlice -> int option * int option) (target: ImageSlice) (canvas: Canvas) : ISL list * int * int =
    let topBlock = canvas.topBlocks |> Map.find "0"
    let canvasArea = float (topBlock.size.width * topBlock.size.height)
    /// Returns instructions, cost, and distance (not scaled)
    let rec solve (blockId: string) (targetSlice: ImageSlice) (candidateColor: Color) : ISL list * int * float =
        let candidateRender = renderBlock (SimpleBlock(blockId, targetSlice.size, {x = 0; y = 0}, candidateColor))
        let distance1 = subImageDistance targetSlice (sliceWholeImage candidateRender)
        let targetArea = targetSlice.size.width * targetSlice.size.height
        let candidates =
            [
                // Option 1: leave color as is (no-op)
                ([], 0, distance1)
            ] @ (
                // Option 2: paint the whole block with the median color
                let medianColor = approxMedianColor targetSlice
                if medianColor = candidateColor then [] else
                let isl2_color = ISL.ColorBlock(blockId, medianColor)
                let cost2_color = int (System.Math.Round (5.0 * canvasArea / float targetArea))
                let isl2, cost2, distance2 = solve blockId targetSlice medianColor
                [(isl2_color :: isl2, cost2_color + cost2, distance2)]
            ) @
            if targetArea <= breakEvenNumberOfPixels || targetSlice.size.width <= 1 || targetSlice.size.height <= 1 then
                []
            else
            // Option 3: split the block into 4 and recurse
            match splitpointSelector targetSlice with
            | None, None -> []
            | Some splitX, None ->
                let cost3_cut = int (System.Math.Round (7.0 * canvasArea / float targetArea))
                if float cost3_cut >= distanceScalingFactor * distance1 then [] else
                let left_slice = subslice targetSlice { width = splitX; height = targetSlice.size.height } { x = 0; y = 0 }
                let right_slice = subslice targetSlice { width = targetSlice.size.width - splitX; height = targetSlice.size.height } { x = splitX; y = 0 }
                let (isl3_left, cost3_left, distance3_left) = solve $"{blockId}.0" left_slice candidateColor
                let (isl3_right, cost3_right, distance3_right) = solve $"{blockId}.1" right_slice candidateColor
                let isl3_cut = ISL.LineCut(blockId, V, splitX + targetSlice.offset.x)
                [(isl3_cut :: isl3_left @ isl3_right, cost3_cut + cost3_left + cost3_right, distance3_left + distance3_right)]
            | None, Some splitY ->
                let cost3_cut = int (System.Math.Round (7.0 * canvasArea / float targetArea))
                if float cost3_cut >= distanceScalingFactor * distance1 then [] else
                let top_slice = subslice targetSlice { width = targetSlice.size.width; height = targetSlice.size.height - splitY } { x = 0; y = splitY }
                let bottom_slice = subslice targetSlice { width = targetSlice.size.width; height = splitY } { x = 0; y = 0 }
                let (isl3_top, cost3_top, distance3_top) = solve $"{blockId}.1" top_slice candidateColor
                let (isl3_bottom, cost3_bottom, distance3_bottom) = solve $"{blockId}.0" bottom_slice candidateColor
                let isl3_cut = ISL.LineCut(blockId, H, splitY + targetSlice.offset.y)
                [(isl3_cut :: isl3_top @ isl3_bottom, cost3_cut + cost3_top + cost3_bottom, distance3_top + distance3_bottom)]
            | Some splitX, Some splitY ->
                let cost3_cut = int (System.Math.Round (10.0 * canvasArea / float targetArea))
                if float cost3_cut >= distanceScalingFactor * distance1 then [] else
                let slice0 = subslice targetSlice { width = splitX; height = splitY } { x = 0; y = 0 }
                let slice1 = subslice targetSlice { width = targetSlice.size.width - splitX; height = splitY } { x = splitX; y = 0 }
                let slice2 = subslice targetSlice { width = targetSlice.size.width - splitX; height = targetSlice.size.height - splitY } { x = splitX; y = splitY }
                let slice3 = subslice targetSlice { width = splitX; height = targetSlice.size.height - splitY } { x = 0; y = splitY }
                let (isl3_0, cost3_0, distance3_0) = solve $"{blockId}.0" slice0 candidateColor
                let (isl3_1, cost3_1, distance3_1) = solve $"{blockId}.1" slice1 candidateColor
                let (isl3_2, cost3_2, distance3_2) = solve $"{blockId}.2" slice2 candidateColor
                let (isl3_3, cost3_3, distance3_3) = solve $"{blockId}.3" slice3 candidateColor
                let isl3_cut = ISL.PointCut(blockId, { x = splitX + targetSlice.offset.x; y = splitY + targetSlice.offset.y })
                [(
                    isl3_cut :: isl3_0 @ isl3_1 @ isl3_2 @ isl3_3,
                    cost3_cut + cost3_0 + cost3_1 + cost3_2 + cost3_3,
                    distance3_0 + distance3_1 + distance3_2 + distance3_3
                )]
        List.minBy (fun (_, cost, distance) -> float cost + distanceScalingFactor * distance) candidates
    let isl, cost, distance = solve "0" target {r = 255uy; g = 255uy; b = 255uy; a = 255uy}
    (isl, cost, int (System.Math.Round (distanceScalingFactor * distance)))
