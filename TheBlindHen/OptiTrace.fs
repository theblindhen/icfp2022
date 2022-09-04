module OptiTrace

open Model
open Util
open Instructions

/// For each color of a block appearing inside the given id, sum up the coloring
/// cost of those blocks
let colorCostsInside canvas (simpleBlocks: Map<string, SimpleBlock>) (id: string) : Map<Color, int> =
    simpleBlocks
    |> Map.filter (fun subId _ -> subId.StartsWith id)
    |> Map.values
    |> List.ofSeq
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
    let optiFinalCanvas, optiCost = simulate initCanvas optimized
    assert (Set(Map.keys simpleBlocks) = Set(Map.keys optiFinalCanvas.topBlocks))
    for id, block in Map.toSeq optiFinalCanvas.topBlocks do
        let block = block :?> SimpleBlock
        let origBlock = simpleBlocks[id]
        assert(origBlock.color = block.color)
        assert(origBlock.size = block.size)
        assert(origBlock.lowerLeft = block.lowerLeft)
    printfn "optimizer %s cost: original cost: %d, optimized cost: %d"
        (if originalCost > optiCost then "IMPROVED" else "WORSENED")
        originalCost
        optiCost
    if originalCost < optiCost then
        isl
    else
        optimized