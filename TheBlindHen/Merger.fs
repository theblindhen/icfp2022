module Merger

open Model
open Util
open Instructions

let cellToPosId (gridInfo: GridInfo) (blockMap: Map<int, Block>) =
    positionsFromBlockMap blockMap
    |> Map.toSeq
    |> Seq.map (fun (posId, (pos, _)) -> {x=pos.x/gridInfo.cellSize.width;y=pos.y/gridInfo.cellSize.height}, posId)
    |> Map.ofSeq

/// Given a list of blocks, return the instructions for merging them
let mergeBlocks maxTopId blockIds =
    let mutable maxTopId = maxTopId
    let isl =
        List.fold (fun (isl, prevId) curId ->
            let merge = ISL.MergeBlocks(prevId, curId)
            maxTopId <- maxTopId + 1
            (merge::isl, string maxTopId)
        ) ([], List.head blockIds) (List.tail blockIds)
        |> fst
        |> List.rev
    (isl, maxTopId)

/// Assume the canvas is sub-divided into a grid of blocks.
/// Merge each `step` adjacent blocks in the given direction.
/// If `step` does not divide the grid size evenly, then `offset`
/// determines how many blocks to skip before starting the merge.
let mergeGrid (canvas: Canvas) (dir: Direction) (step: int) (offset: int) : (ISL list * int list * int) =
    match canvasGridInfo canvas with
    | None -> failwith "Canvas is not a grid"
    | Some gridInfo ->
    let blockMap = blockMap canvas
    let cellToPosId = cellToPosId gridInfo blockMap
    let mutable maxTopId = canvas.maxTopId
    let mutable mergedLinesIds = []
    if dir = H then
        let isl =
            [ for y in 0 .. gridInfo.dimensions.height-1 do
                for dx in 0 .. gridInfo.dimensions.width/step-1 do
                    let merges, maxTopId' = mergeBlocks maxTopId [for i in 0 .. step-1 -> blockMap[cellToPosId[{x=dx*step+i+offset; y=y}]].id]
                    maxTopId <- maxTopId'
                    mergedLinesIds <- maxTopId :: mergedLinesIds
                    yield! merges ]
            |> List.ofSeq
        (isl, List.rev mergedLinesIds, maxTopId)
    else
        failwith "Not implemented"


let mergeMetaSolver (solver: Solver) (target: Image) (canvas: Canvas) =
    let bestStep, instructions, _ =
        [ for step in 2 .. 5 do
            let mergeInstructions,_,_ = mergeGrid canvas H step 0
            let (mergedCanvas,_) = simulate canvas mergeInstructions
            let solverInstructions, _ = solver target mergedCanvas
            yield (step, mergeInstructions @ solverInstructions) ]
        |> List.map (fun (step, instructions) ->
            let (_, cost) = simulate canvas instructions
            (step, instructions, cost))
        |> List.maxBy (fun (_, _, cost) -> cost)
    printfn "Best step: %d" bestStep
    (instructions, None)

let mergeAll (canvas: Canvas) : (ISL list * int) =
    match canvasGridInfo canvas with
    | None -> failwith "Canvas is not a grid"
    | Some gridInfo ->
    // printfn "Grid info: %A" gridInfo
    let mergedRows, rowIds, maxTopId = mergeGrid canvas H gridInfo.dimensions.width 0
    let mergedCols, maxTopId = mergeBlocks maxTopId (List.map string rowIds)
    (mergedRows @ mergedCols, maxTopId)

let mergeAllMetaSolver (solver: Solver) (optiTrace: bool) (target: Image) (canvas: Canvas) =
    let mergeInstructions,lastBlockId = mergeAll canvas
    let colorInstr = ISL.ColorBlock (string lastBlockId, { r=255; g=255; b=255; a=255 })
    let mergeAndColor = mergeInstructions @ [colorInstr]
    // printfn "Merge instructions:\n%s" (deparse mergeInstructions)
    let (mergedCanvas,_) = simulate canvas mergeAndColor
    printfn "Ran merging!"
    let solverInstructions, _ = solver target mergedCanvas
    if optiTrace then
        printfn "Running Optimizer on post-merge instructions"
        let optiSolverInstructions = OptiTrace.optimize target mergedCanvas solverInstructions
        (mergeAndColor @ optiSolverInstructions, None)
    else
        (mergeAndColor @ solverInstructions, None)