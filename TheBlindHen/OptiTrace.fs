module OptiTrace

open Model
open Util
open Instructions

/// Return all blocks that are inside the given block id
/// In the absence of swap and merge, this is equivalent to having an id that
/// starts with the given id.
let blocksInside (simpleBlocks: Map<string, 'a :> Block>) (id: string) : 'a list =
    Map.values simpleBlocks
    |> Seq.filter (fun (b: 'a) -> b.id.StartsWith id)
    |> List.ofSeq

/// For each color of a block appearing inside the given id, sum up the coloring
/// cost of those blocks
let colorCostsInside canvas (simpleBlocks: Map<string, SimpleBlock>) (id: string) : Map<Color, int> =
    blocksInside simpleBlocks id
    |> List.groupBy (fun block -> block.color)
    |> List.map (fun (color, blocks) ->
        let cost =
            blocks
            |> List.map (fun b -> Instructions.islCost canvas Instructions.ISLOps.ColorBlock b.size)
            |> List.sum
        color, cost)
    |> Map.ofList

/// Assume a trace that includes only cuts and colors.
/// Run the same cuts as the given trace, and in the same order, but instead of
/// emitting the Color instructions in the trace as they are encountered, always
/// precede a cut operation with a Color instruction that colors the smallest
/// block contained in the cut block.
let optiColorTraceNaive (target: Image) (initCanvas: Canvas) (isl: ISL list) =
    let finalCanvas, originalCost = simulate initCanvas isl
    let simpleBlocks = Map.map (fun _ (b: Block) -> b :?> SimpleBlock) finalCanvas.topBlocks 
    let optimized =
        // The first pass contains all cuts and interleaved colors
        let firstPass =
            List.fold (fun (canvas, islRev) instruction ->
                let newInstructions = 
                    match instruction with
                    | ISL.ColorBlock _ -> []
                    | ISL.LineCut (id,_,_) | ISL.PointCut (id,_) ->
                        let colorCosts = colorCostsInside canvas simpleBlocks id
                        let maxColor =
                            colorCosts
                            |> Map.toSeq
                            |> Seq.maxBy (fun (_,cost) -> cost)
                            |> fst
                        if maxColor = (canvas.topBlocks[id] :?> SimpleBlock).color then
                            [ instruction ]
                        else
                            [ ISL.ColorBlock(id, maxColor); instruction ]
                    | _ -> failwith "optiCutAndColorTrace: unsupported instruction"
                (fst (simulate canvas newInstructions), (List.rev newInstructions) @ islRev)
            ) (initCanvas, []) isl
            |> snd
            |> List.rev
        let canvas, _ = simulate initCanvas firstPass
        // Now find all blocks which have an incorrect color
        let afterColors =
            Map.toSeq canvas.topBlocks
            |> Seq.map (fun (id, block) ->
                let block = block :?> SimpleBlock
                let targetColor = simpleBlocks[id].color
                if block.color = targetColor then
                    None
                else
                    Some (ISL.ColorBlock(id, targetColor)))
            |> Seq.choose id
            |> List.ofSeq
        firstPass @ afterColors
    // Check that we didn't do anything wrong:
    // All blocks should end up with the same colors (and ids and sizes) that
    // they had originally
    let optiFinalCanvas, optiCost = simulate initCanvas optimized
    assert (Set(Map.keys simpleBlocks) = Set(Map.keys optiFinalCanvas.topBlocks))
    for id, block in Map.toSeq optiFinalCanvas.topBlocks do
        let block = block :?> SimpleBlock
        let origBlock = simpleBlocks[id]
        assert(origBlock.color = block.color)
        assert(origBlock.size = block.size)
        assert(origBlock.lowerLeft = block.lowerLeft)
    optimized

/// Go through all Color instructions. For each, enumerate the final
/// simpleBlocks that are colored by this instruction. Replace the Color by the
/// median color appearing in all those blocks.
let optimizeColors (target: Image) (initCanvas: Canvas) (isl: ISL list) =
    let finalCanvas, originalCost = simulate initCanvas isl
    let simpleBlocks = Map.map (fun _ (b: Block) -> b :?> SimpleBlock) finalCanvas.topBlocks 
    let isl = Array.ofList isl
    for i = 0 to isl.Length - 1 do
        match isl.[i] with
        | ISL.ColorBlock (blockId, oldColor) ->
            // Go through the remaining instructions to determine which final
            // sub-blocks are colored by this instruction.
            let recoloredSubs: string list =
                isl.[i+1..]
                |> Seq.ofArray
                |> Seq.map (fun instruction ->
                    match instruction with
                    | ISL.ColorBlock (subId, _) ->
                        if subId.StartsWith blockId then Some subId
                        else None
                    | _ -> None)
                |> Seq.choose id
                |> List.ofSeq
            let untouchedSubs =
                recoloredSubs
                :: (recoloredSubs |> List.map (fun subId ->
                                        blocksInside simpleBlocks subId
                                        |> List.map (fun b -> b.id)))
                |> List.concat
                |> Set.ofList
            let touchedSubs =
                Set.difference
                    (blocksInside simpleBlocks blockId
                      |> List.map (fun b -> b.id)
                      |> Set.ofList)
                    untouchedSubs
            let medianColor =
                touchedSubs
                |> Set.toSeq
                |> Seq.map (fun subId ->
                    let sub = simpleBlocks[subId]
                    sliceImage target sub.size sub.lowerLeft
                    |> imageSlicePixels)
                |> Seq.concat
                |> medianColorSeq
            if medianColor <> oldColor then
                printfn "Color %s changed from %s to %s" blockId (oldColor.toString()) (medianColor.toString())
                isl.[i] <- ISL.ColorBlock (blockId, medianColor)
        | _ -> ()
    Array.toList isl

let chooseBest (target: Image) (initCanvas: Canvas) (solutions: (string * ISL list) list) =
    solutions
    |> List.map (fun (name, solution) ->
        let (cost, similarity) = scoreSolution target initCanvas solution
        (cost, similarity, name, solution))
    |> List.minBy (fun (cost, similarity, _,  _) -> cost+similarity)

let optimize (target: Image) (initCanvas: Canvas) (originalSolution: ISL list) =
    let optiColors = optimizeColors target initCanvas originalSolution
    let optiColorTrace = optiColorTraceNaive target initCanvas originalSolution
    let (optiCost, optiSimilarity, bestOpti, optimized) =
        [ ("original", originalSolution)
          ("optiColors", optiColors)
          ("optiColorTrace", optiColorTrace) ]
        |> chooseBest target initCanvas
    if bestOpti = "original" then
        printfn "Optimizer could not improve"
    else
        let (originalCost, originalSimilarity) = scoreSolution target initCanvas originalSolution
        let originalPenalty = originalCost + originalSimilarity
        printfn "Optimizer IMPROVED, choosing %s" bestOpti
        printfn "\tOriginal:  %d\t(ISL=%d\tSim=%d)\n\tOptimized: %d\t(ISL=%d\tSim=%d)"
            originalPenalty originalCost originalSimilarity
            (optiCost + optiSimilarity) optiCost optiSimilarity
    optimized