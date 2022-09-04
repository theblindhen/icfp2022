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

/// Determine whether our score may be improved by coloring the input simpleblock
/// according to the median color of the target. If so, return that instruction.
/// Otherwise return the empty instruction list
let colorBlockMedianIfBeneficial (target: ImageSlice) (canvas: Canvas) (block: SimpleBlock) : ISL list =
    let currentSimilarity = singleColorSimilarity block.color target
    let median = medianColor target
    let medianSimilarity = singleColorSimilarity median target
    let colorCost = islCost canvas ISLOps.ColorBlock block.size
    if currentSimilarity > medianSimilarity + colorCost then
        [ ISL.ColorBlock(block.id, median) ]
    else
        []

/// Go through each block in the canvas and decide whether it should be colored
/// according to the median color of the target image.
let colorCanvasMedianWhereBeneficial (target: Image) (canvas: Canvas) =
    [ for block in canvas.topBlocks.Values do
        if block :? SimpleBlock then
            let block = block :?> SimpleBlock
            let targetSlice = sliceImage target block.size block.lowerLeft
            yield! colorBlockMedianIfBeneficial targetSlice canvas block ]

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

type SplitPointSelector = ImageSlice -> int option * int option

/// Returns instructions, cost, and similarity (scaled)
let quadtreeSolver (splitpointSelector: SplitPointSelector) (target: ImageSlice) (canvas: Canvas) : ISL list * int * int =
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
    let isl, cost, distance = solve "0" target {r = 255; g = 255; b = 255; a = 255}
    (isl, cost, int (System.Math.Round (distanceScalingFactor * distance)))

let randomSemiNormalBetween (lowerBound: int) (upperBound: int) =
    assert (lowerBound <> upperBound)
    let x1 = Rng.rng.NextDouble()
    let x2 = Rng.rng.NextDouble()
    let x3 = Rng.rng.NextDouble()
    let result = int ((x1 + x2 + x3) / 3.0 * (float (upperBound - lowerBound)) + float lowerBound)
    if result >= upperBound then upperBound - 1 else result

/// Returns instructions, cost, and similarity (scaled)
let fastRandomSolver (target: ImageSlice) (canvas: Canvas) : ISL list * int * int =
    let topBlock = canvas.topBlocks |> Map.find "0"
    let canvasArea = float (topBlock.size.width * topBlock.size.height)
    /// Returns instructions, cost, and distance (not scaled)
    /// The candidateColor parameter is the color of the parent block.
    let rec solve (blockId: string) (targetSlice: ImageSlice) (candidateColor: Color) : ISL list * int * float =
        let targetArea = targetSlice.size.width * targetSlice.size.height
        let currentDistance = approxSingleColorDistance candidateColor targetSlice
        if targetArea <= 79 then ([], 0, currentDistance) else // Even in the best case, coloring is too expensive
        let colorCost = int (System.Math.Round (5.0 * canvasArea / float targetArea))
        let candidates =
            [
                // Option 1: leave color as is (no-op)
                ([], 0, currentDistance)
            ] @ (
                // Option 2: paint the whole block with the "median" color
                let medianColor = approxAverageColor targetSlice
                if medianColor = candidateColor then [] else
                let medianDistance = approxSingleColorDistance medianColor targetSlice
                let isl2_color = ISL.ColorBlock(blockId, medianColor)
                [([isl2_color], colorCost, medianDistance)]
            ) @
            if targetArea <= breakEvenNumberOfPixels || targetSlice.size.width <= 1 || targetSlice.size.height <= 1 then
                []
            else
            // Option 3: split the block into 4 and recurse
            // TODO: perf: have a heuristic to avoid computing option 3 at all. Otherwise it's probably too slow.
            // TODO: color before splitting in many/all cases.
            let splitX = randomSemiNormalBetween 1 targetSlice.size.width
            let splitY = randomSemiNormalBetween 1 targetSlice.size.height
            let cost3_cut = int (System.Math.Round (10.0 * canvasArea / float targetArea))
            let largestSlice =
                // If we can't color at least the largest slice after splitting,
                // there's no point in splitting in the first place.
                let largestWidth = max splitX (targetSlice.size.width - splitX)
                let largestHeight = max splitY (targetSlice.size.height - splitY)
                largestWidth * largestHeight
            let cost3_paintLargest = int (System.Math.Round (5.0 * canvasArea / float largestSlice))
            if cost3_cut + cost3_paintLargest >= distanceToSimilarity currentDistance then [] else
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
        List.minBy (fun (_, cost, distance) -> cost + distanceToSimilarity distance) candidates
    let isl, cost, distance = solve "0" target {r = 255; g = 255; b = 255; a = 255}
    (isl, cost, distanceToSimilarity distance)

type MCTSState = {
    instructionsRev: ISL list
    instructionCost: int
    canvas: Canvas
    blocksControlled: Set<string>
}

type MCTSAction = 
    | MidpointCut of Block
    | PaintMedian of Block

/// Pick n random elements from a sequence
let pickRandomN (n: int) (seq: seq<'a>) =
    if Seq.length seq <= n then seq else
    seq
    |> Seq.map (fun x -> (Rng.rng.Next(), x))
    |> Seq.sortBy fst
    |> Seq.take n
    |> Seq.map snd

let actions state =
    state.blocksControlled
    |> Seq.map (fun blockId -> Map.find blockId state.canvas.topBlocks)
    |> Seq.filter (fun block ->
        block.size.width > 1 &&
        block.size.height > 1 &&
        block.size.width * block.size.height > breakEvenNumberOfPixels)
    |> pickRandomN 10
    |> Seq.map (fun block -> [MidpointCut block; PaintMedian block])
    |> Seq.concat
    |> Array.ofSeq

let step targetImage state action =
    match action with
    | PaintMedian block ->
        let targetSlice = subslice targetImage block.size block.lowerLeft
        let medianColor = approxMedianColor targetSlice
        let isl = ISL.ColorBlock(block.id, medianColor)
        let canvas, cost = simulate_step state.canvas isl
        {
            instructionsRev = isl :: state.instructionsRev
            instructionCost = cost + state.instructionCost
            canvas = canvas
            blocksControlled = state.blocksControlled
        }
    | MidpointCut block ->
        let isl = ISL.PointCut(block.id, { x = block.lowerLeft.x + block.size.width / 2; y = block.lowerLeft.y + block.size.height / 2 })
        let canvas, cost = simulate_step state.canvas isl
        {
            instructionsRev = isl :: state.instructionsRev
            instructionCost = cost + state.instructionCost
            canvas = canvas
            blocksControlled = state.blocksControlled.Remove(block.id).Add($"{block.id}.0").Add($"{block.id}.1").Add($"{block.id}.2").Add($"{block.id}.3")
        }

// Take random actions until we reach a state from which no actions are available
let simulate targetImage state =
    let mutable i = 0
    let rec loop state =
        let actions = actions state
        if actions.Length = 0 then state else
        let action = actions.[0] // Actions are already randomized
        let state = step targetImage state action
        i <- i + 1
        loop state
    let endState = loop state
    let distance =
        Seq.sumBy (fun blockId ->
            let block = Map.find blockId endState.canvas.topBlocks
            let targetSlice = subslice targetImage block.size block.lowerLeft
            let renderedBlock = renderBlock block
            subImageDistance (sliceWholeImage renderedBlock) targetSlice
        ) endState.blocksControlled
    let similarity = int (System.Math.Round (distance * distanceScalingFactor))
    similarity + endState.instructionCost

/// Returns instructions, cost, and similarity (scaled)
let mctsSolver (target: ImageSlice) (originalCanvas: Canvas) =
    /// Returns reversed(!) instructions, cost, and distance (unscaled!)
    let rec solveBlock canvas blockId =
        let state = {
            instructionsRev = []
            instructionCost = 0
            canvas = canvas
            blocksControlled = Set.singleton blockId
        }
        match MCTS.mctsFindBestMove (actions) (step target) (simulate target) (sqrt 2.0) (1_000) state with
        | None ->
            let block = Map.find blockId canvas.topBlocks
            let targetSlice = subslice target block.size block.lowerLeft
            let renderedBlock = renderBlock block
            let distance = subImageDistance (sliceWholeImage renderedBlock) targetSlice
            ([], 0, distance)
        | Some (_, state) ->
            printfn "Recursing into %d blocks" state.blocksControlled.Count
            // Recurse into the controlled blocks of the new state
            Seq.fold (fun (instructionsRev, cost, distance) blockId ->
                let (instructionsRev', cost', distance') = solveBlock state.canvas blockId
                (instructionsRev' @ instructionsRev, cost' + cost, distance' + distance)
            ) (state.instructionsRev, state.instructionCost, 0.0) state.blocksControlled
    let (islRev, cost, distance) = solveBlock originalCanvas "0"
    (List.rev islRev, cost, int (System.Math.Round (distanceScalingFactor * distance)))
