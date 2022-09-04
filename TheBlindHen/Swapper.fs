module Swapper

open Model
open Instructions

let assertAllBlockSameSize canvas =
    let b0 = canvas.topBlocks["0"]
    for b in Map.values canvas.topBlocks do
        assert (b.size = b0.size)

/// Create a permanent numbering of each location of a block in the canvas.
/// This is because the blockId's will be moved around when swapping
let positionMap (canvas: Canvas) =
    let (_, blockMap, positions) =
        Map.values canvas.topBlocks
        |> List.ofSeq
        |> List.sortBy (fun b -> b.lowerLeft.y * canvas.size.width + b.lowerLeft.x)
        |> Seq.fold (fun (n, blockMap, positionMap) block ->
                (n+1, Map.add n (block :?> SimpleBlock) blockMap, Map.add block.lowerLeft n positionMap)
            ) (0, Map.empty, Map.empty)
    (blockMap, positions)

let swapBlocks (blockMap: Map<int, SimpleBlock>) pos1 pos2 =
    let b1, b2 = blockMap[pos1], blockMap[pos2]
    let blockMap = 
        blockMap
        |> Map.add pos2 (SimpleBlock(b1.id, b1.size, b2.lowerLeft, b1.color))
        |> Map.add pos1 (SimpleBlock(b2.id, b2.size, b1.lowerLeft, b2.color))
    (blockMap, ISL.SwapBlocks(b1.id, b2.id))

let eagerSwapper (targetImage: Image) (canvas: Canvas) =
    assertAllBlockSameSize canvas
    let blockMap,_ = positionMap canvas
    let chooseGreedySwap (blockMap: Map<int, SimpleBlock>) posId =
        let curBlock = blockMap[posId]
        let curTarget = sliceImage targetImage curBlock.size curBlock.lowerLeft
        let distToCur posId =
            Util.singleColorDistance (blockMap[posId].color) curTarget
        let curDist = distToCur posId
        let bestSwap =
            Map.keys blockMap
            |> Seq.map (fun swapPosId ->
                if swapPosId <= posId then
                    None // Only swap higher id stuff
                else
                    Some (swapPosId, distToCur swapPosId))
            |> Seq.minBy (function
                | None -> infinity
                | Some (_, dist) -> dist)
        match bestSwap with
        | Some (swapBlockId, swapDist) when swapDist < curDist ->
            Some (swapBlockId, swapDist)
        | _ -> None
    let instructions =
        Map.keys blockMap
        |> List.ofSeq
        |> List.sort // In order of position id so we only swap up
        |> List.fold (fun (blockMap, swaps) posId ->
            // Map.iter (fun id (block: SimpleBlock) ->
            //     printfn "\t\t %s: %s" id (block.color.toString())) blockMap
            match chooseGreedySwap blockMap posId with
            | None -> (blockMap, swaps)
            | Some (swapPosId,_) ->
                let blockMap, swap = swapBlocks blockMap posId swapPosId
                (blockMap, swap :: swaps)
                )
            (blockMap, [])
        |> snd
        |> List.rev
    (instructions, None)

let computeColorAssignment (colorToBlockIds: Map<Color, string list>) (posColorCosts: Map<int, Map<Color, float>>) : Map<int, Color> =
    // Set up mixed-integer linear program using Flips
    let colors = Map.keys colorToBlockIds |> List.ofSeq
    let positions = Map.keys posColorCosts |> List.ofSeq
    let iColorAtPosName (posId: int) (color: Color) = sprintf "iColorAtPos_%d_%s" posId (color.toString())
    let iColorAtPos =
        [ for posId in Map.keys posColorCosts do
            for color in Map.keys colorToBlockIds do
                ((posId, color), Flips.Decision.createBoolean (iColorAtPosName posId color)) ]
        |> Map.ofList
    let objectiveExpression = List.sum [
        for (posId, color) in Map.keys iColorAtPos ->
            iColorAtPos[posId, color] * posColorCosts[posId][color] ]
    // Constraint: each position must have exactly one color
    let posConstraints = [
        for posId in positions ->
            Flips.Constraint.create
                $"constr[{posId}]assigned"
                (List.sum [ for color in colors -> 1.*iColorAtPos[(posId, color)] ] >==  0.99) ]
    // Constraint: each color must be assigned at most the original number of
    // times
    let colConstraints = [
        for color in colors ->
            Flips.Constraint.create
                $"constr[{color}]limit"
                (List.sum [ for posId in positions -> 1.*iColorAtPos[(posId, color)] ] <==  float(List.length colorToBlockIds[color]) + 0.001) ]
    // Set up model
    let model = 
        Flips.Model.create (Flips.Objective.create "MinimizeColorDist" Flips.Types.Minimize objectiveExpression)
        |> Flips.Model.addConstraints (posConstraints @ colConstraints)
    let settings = {
        Flips.Types.SolverType = Flips.Types.SolverType.CBC
        Flips.Types.MaxDuration = 10_000L
        Flips.Types.WriteLPFile = None
        Flips.Types.WriteMPSFile = None
    }
    match Flips.Solver.solve settings model with
    | Flips.Types.Optimal solution ->
        positions
        |> Seq.map (fun posId ->
            let assColor = colors |> List.find (fun color -> solution.DecisionResults[iColorAtPos[(posId,color)]] > 0.1)
            (posId, assColor))
        |> Map.ofSeq
    | _ -> failwith "Failed to find optimal solution"

/// Assert that each color is used the correct number of times
let assertConformalAssignment (colorToBlockIds: Map<Color, string list>) (assignment: Map<int, Color>) =
    for color in Map.keys colorToBlockIds do
        let assigned = assignment |> Map.filter (fun _ c -> c = color) |> Map.keys |> List.ofSeq
        let expected = colorToBlockIds[color]
        if List.length assigned <> List.length expected then
            failwithf "Color %s assigned %d times, expected %d" (color.toString()) (List.length assigned) (List.length expected)

let swapsFromAssignment (blockMap: Map<int, SimpleBlock>) (assignment: Map<int, Color>) : Instructions.ISL list =
    let positions = Map.keys blockMap |> List.ofSeq |> List.sort
    assignment
    |> Map.toSeq
    |> Seq.fold (fun (blockMap: Map<int, SimpleBlock>, swaps) (posId, assColor) ->
        let currentColor = blockMap[posId].color
        if currentColor = assColor then
            (blockMap, swaps)
        else
            let swapPosId =
                // Look for a later position that has a block of the correct
                // color and whose assignment is the current color.
                // If no such exists, just choose one of the correct color.
                let laterPositions = positions |> List.filter (fun p -> p > posId) in
                match List.tryFind (fun id -> blockMap[id].color = assColor && assignment[id] = currentColor) laterPositions with
                | Some p -> p
                | None ->
                    match List.tryFind (fun id -> blockMap[id].color = assColor) laterPositions with
                    | Some p -> p
                    | None -> failwith "No position found to swap with"
            let (blockMap, swap) = swapBlocks blockMap posId swapPosId
            (blockMap, swap :: swaps)
        ) (blockMap, [])
    |> snd
    |> List.rev

let assignSwapper (targetImage: Image) (canvas: Canvas) =
    assertAllBlockSameSize canvas
    let blockMap, _ = positionMap canvas
    let colorToBlockIds =
        Map.values canvas.topBlocks
        |> Seq.map (fun b -> (b :?> SimpleBlock).color, b.id)
        |> Seq.groupBy (fun (color, _) -> color)
        |> Seq.map (fun (color, idList) -> color, idList |> Seq.map snd |> List.ofSeq)
        |> Map.ofSeq
    let colors = Map.keys colorToBlockIds |> List.ofSeq
    let posColorCosts =
        blockMap
        |> Map.map (fun posId block ->
            let target = sliceImage targetImage block.size block.lowerLeft
            colors
            |> List.map (fun color ->
                let cost = Util.singleColorDistance color target
                color, cost)
            |> Map.ofSeq)
    let assignment = computeColorAssignment colorToBlockIds posColorCosts
    // printfn "Assignments:"
    // assignment |> Map.iter (fun posId color ->
    //     printfn "\t%d assigned color %s" posId (color.toString()))
    assertConformalAssignment colorToBlockIds assignment
    (swapsFromAssignment blockMap assignment, None)