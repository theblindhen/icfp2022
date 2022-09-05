module Merger

open Model
open Util
open Instructions

/// Assume the canvas is sub-divided into a grid of blocks.
/// Merge each `step` adjacent blocks in the given direction.
/// If `step` does not divide the grid size evenly, then `offset`
/// determines how many blocks to skip before starting the merge.
let mergeGrid (canvas: Canvas) (dir: Direction) (step: int) (offset: int) : ISL list =
    match canvasGridInfo canvas with
    | None -> failwith "Canvas is not a grid"
    | Some gridInfo ->
    let blockMap = blockMap canvas
    let idToPositions = positionsFromBlockMap blockMap
    let cellToPosId =
        idToPositions
        |> Map.toSeq
        |> Seq.map (fun (posId, (pos, _)) -> {x=pos.x/gridInfo.cellSize.width;y=pos.y/gridInfo.cellSize.height}, posId)
        |> Map.ofSeq
    let mutable globalCounter = Map.count canvas.topBlocks
    let mergeBlocks blockIds =
        List.fold (fun (isl, prevId) curId ->
            let merge = ISL.MergeBlocks(prevId, curId)
            globalCounter <- globalCounter + 1
            (merge::isl, string globalCounter)
        ) ([], List.head blockIds) (List.tail blockIds)
        |> fst
        |> List.rev
    if dir = H then
        [ for y in 0 .. gridInfo.dimensions.height-1 do
            for dx in 0 .. gridInfo.dimensions.width/step-1 do
              yield! mergeBlocks [for i in 0 .. step-1 -> blockMap[cellToPosId[{x=dx*step+i+offset; y=y}]].id] ]
        |> List.ofSeq
    else
        failwith "Not implemented"

let mergeMetaSolver (solver: Solver) (target: Image) (canvas: Canvas) =
    let bestStep, instructions, _ =
        [ for step in 2 .. 5 do
            let mergeInstructions = mergeGrid canvas H step 0
            let (mergedCanvas,_) = simulate canvas mergeInstructions
            let solverInstructions, _ = solver target mergedCanvas
            yield (step, mergeInstructions @ solverInstructions) ]
        |> List.map (fun (step, instructions) ->
            let (_, cost) = simulate canvas instructions
            (step, instructions, cost))
        |> List.maxBy (fun (_, _, cost) -> cost)
    printfn "Best step: %d" bestStep
    (instructions, None)