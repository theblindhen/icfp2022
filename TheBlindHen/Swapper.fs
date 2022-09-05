module Swapper

open Model
open Util
open Instructions

let swapBlocks (blockMap: Map<int, SimpleBlock>) pos1 pos2 =
    let b1, b2 = blockMap[pos1], blockMap[pos2]
    let blockMap = 
        blockMap
        |> Map.add pos2 (SimpleBlock(b1.id, b1.size, b2.lowerLeft, b1.color))
        |> Map.add pos1 (SimpleBlock(b2.id, b2.size, b1.lowerLeft, b2.color))
    (blockMap, ISL.SwapBlocks(b1.id, b2.id))

let eagerSwapper (targetImage: Image) (canvas: Canvas) =
    assert (canvasGridInfo canvas <> None)
    let blockMap = blockMap canvas
                   |> Map.map (fun _ (b: Block) -> b :?> SimpleBlock)
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

let computeGroupAssignment (groupToBlockIds: Map<'a, string list>) (posGroupCosts: Map<int, Map<'a, float>>) : Map<int, 'a> =
    // Set up mixed-integer linear program using Flips
    let groups = Map.keys groupToBlockIds |> List.ofSeq
    let positions = Map.keys posGroupCosts |> List.ofSeq
    let iGroupAtPosName (posId: int) (group: 'a) = sprintf "iGroupAtPos_%d_%A" posId (group)
    let iGroupAtPos =
        [ for posId in positions do
            for group in groups do
                ((posId, group), Flips.Decision.createBoolean (iGroupAtPosName posId group)) ]
        |> Map.ofList
    let objectiveExpression = List.sum [
        for (posId, group) in Map.keys iGroupAtPos ->
            iGroupAtPos[posId, group] * posGroupCosts[posId][group] ]
    // Constraint: each position must have exactly one group
    let posConstraints = [
        for posId in positions ->
            Flips.Constraint.create
                $"constr[{posId}]assigned"
                (List.sum [ for group in groups -> 1.*iGroupAtPos[(posId, group)] ] >==  0.99) ]
    // Constraint: each group must be assigned at most the original number of
    // times
    let colConstraints = [
        for group in groups ->
            Flips.Constraint.create
                $"constr[{group}]limit"
                (List.sum [ for posId in positions -> 1.*iGroupAtPos[(posId, group)] ] <==  float(List.length groupToBlockIds[group]) + 0.001) ]
    // Set up model
    let model = 
        Flips.Model.create (Flips.Objective.create "MinimizeGroupDist" Flips.Types.Minimize objectiveExpression)
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
            let assGroup = groups |> List.find (fun group -> solution.DecisionResults[iGroupAtPos[(posId,group)]] > 0.1)
            (posId, assGroup))
        |> Map.ofSeq
    | _ -> failwith "Failed to find optimal solution"

// /// Assert that each color is used the correct number of times
// let assertConformalAssignment (groupToBlockIds: Map<Color, string list>) (assignment: Map<int, Color>) =
//     for color in Map.keys groupToBlockIds do
//         let assigned = assignment |> Map.filter (fun _ c -> c = color) |> Map.keys |> List.ofSeq
//         let expected = groupToBlockIds[color]
//         if List.length assigned <> List.length expected then
//             failwithf "Color %s assigned %d times, expected %d" (color.toString()) (List.length assigned) (List.length expected)

/// Compute a list of swaps that will move the blocks to their assigned groups
/// Arguments:
///    - posToBlockId: position id -> which block is at that position
///    - blockToGroup: block id -> which group it belongs to
///    - posGroupAssignment: position id -> which group should be at that position
let swapsFromAssignment (posToBlockId: Map<int, string>) (blockToGroup: Map<string, 'a>) (posGroupAssignment: Map<int, 'a>) : Instructions.ISL list =
    let positions = Map.keys posToBlockId |> List.ofSeq |> List.sort
    posGroupAssignment 
    |> Map.toSeq
    |> List.ofSeq
    |> List.sortBy (fun (posId, _) -> posId)
    |> List.fold (fun (posToBlockId: Map<int, string>, swaps) (posId, assGroup) ->
        let currentBlockId = posToBlockId[posId]
        let currentGroup = blockToGroup[currentBlockId]
        if currentGroup = assGroup then
            (posToBlockId, swaps)
        else
            let swapPosId =
                // Look for a later position that has a block of the correct
                // group and whose assignment is the current group.
                // If no such exists, choose one of the correct group which is
                // not already placed at a correct position.
                let laterPositions = positions |> List.filter (fun otherPos -> otherPos > posId) in
                match laterPositions |> List.tryFind (fun otherPos ->
                    blockToGroup[posToBlockId[otherPos]] = assGroup && posGroupAssignment[otherPos] = currentGroup) with
                | Some otherPos -> otherPos
                | None ->
                    match laterPositions |> List.tryFind (fun otherPos ->
                        let otherGroup= blockToGroup[posToBlockId[otherPos]]
                        otherGroup = assGroup && otherGroup <> posGroupAssignment[otherPos]) with
                    | Some otherPos -> otherPos
                    | None -> failwith "No position found to swap with"
            let swapBlockId = posToBlockId[swapPosId]
            let posToBlockId = posToBlockId |> Map.add posId swapBlockId |> Map.add swapPosId currentBlockId
            (posToBlockId, ISL.SwapBlocks(currentBlockId, swapBlockId)  :: swaps)
        ) (posToBlockId, [])
    |> snd
    |> List.rev

type SwapperStrategyOutput<'a when 'a:comparison> = (
        Map<string, 'a> * // blockToGroup
        Map<int, Map<'a, float>> // posGroupCosts
    )

/// Takes a block map and returns the necessary groupings and costs for swapper strategy
type SwapperStrategy<'a when 'a:comparison> = ImageSlice -> Canvas -> Map<int, Block> -> SwapperStrategyOutput<'a>

let simpleGridSwapping (targetImage: ImageSlice) (canvas: Canvas) (blockMap: Map<int, Block>) : SwapperStrategyOutput<int> =
    let blockToGroup =
        blockMap
        |> Map.values
        |> Seq.map (fun (b: Block) -> (b.id, (b :?> SimpleBlock).color.asInt ()))
        |> Map.ofSeq
    let positions = positionsFromBlockMap blockMap
    let colorToBlockIds =
        Map.values canvas.topBlocks
        |> Seq.map (fun b -> (b :?> SimpleBlock).color, b.id)
        |> Seq.groupBy (fun (color, _) -> color)
        |> Seq.map (fun (color, idList) -> color, idList |> Seq.map snd |> List.ofSeq)
        |> Map.ofSeq
    let groupToColor =
        colorToBlockIds
        |> Map.keys
        |> Seq.map (fun color -> color.asInt (), color)
        |> Map.ofSeq
    let groups = Map.keys groupToColor |> List.ofSeq
    // For each position, what would it cost to have the median color there
    let medianColorCosts =
        positions
        |> Map.toSeq
        |> Seq.map (fun (posId, (lowerLeft, size)) ->
            let imageSlice = subslice targetImage size lowerLeft
            let medianColor = Util.medianColor imageSlice
            let similarityCost = Util.singleColorSimilarity medianColor imageSlice
            let colorizeCost = islCost canvas ISLOps.ColorBlock size
            (posId, float (similarityCost + colorizeCost)))
        |> Map.ofSeq
    // The cost of having a particular color at that position
    // This is computed as
    //     min(cost of having that color, cost of having the median color)
    //   + estimated cost of swapping that color in at that position
    // The cost of having the median color includes the distance and the
    // instruction cost of assigning the median value.
    // The estimated cost of swapping is 0 if the color is already present,
    // and a heuristic multiplied by the swap instruction cost otherwise.
    // TODO: Our estimate of swapping cost may be improved by factoring in
    // the number of positions of the current and the target color
    let posGroupCosts =
        let swapCostMultiplier = 0.6 //1.0 - 1.0/float(List.length groups)
        blockMap
        |> Map.map (fun posId block ->
            let target = subslice targetImage block.size block.lowerLeft
            groups
            |> List.map (fun group ->
                let color = groupToColor[group]
                let thisColorCost = float (Util.singleColorSimilarity color target)
                let medianColorCost = float medianColorCosts[posId]
                let swapCost =
                    if (block :?> SimpleBlock).color = color then 0.
                    else swapCostMultiplier * float (islCost canvas ISLOps.SwapBlocks block.size)
                group, swapCost + System.Math.Min (thisColorCost, medianColorCost))
            |> Map.ofSeq)
    blockToGroup, posGroupCosts

/// Swapper that assumes that all blocks are simple
let assignSwapperStrategy<'a when 'a:comparison> (swapperStrategy: SwapperStrategy<'a>) (targetImage: Image) (canvas: Canvas) : (ISL list * (int * int) option) =
    let blockMap = blockMap canvas
    let blockToGroup, posGroupCosts = swapperStrategy (sliceWholeImage targetImage) canvas blockMap
    let groupToBlockIds =
        blockToGroup
        |> Map.toSeq
        |> Seq.map (fun (blockId, group) -> group, blockId)
        |> List.ofSeq
        |> List.groupBy (fun (group, _) -> group)
        |> List.map (fun (group, idList) -> group, idList |> List.map snd)
        |> Map.ofSeq
    let posToBlockId = Map.map (fun _ (b: Block) -> b.id) blockMap
    let assignment = computeGroupAssignment groupToBlockIds posGroupCosts
    let swaps = swapsFromAssignment posToBlockId blockToGroup assignment

    let (postSwapCanvas, _) = simulate canvas swaps
    let postColorize = AI.colorCanvasMedianWhereBeneficial targetImage postSwapCanvas
    printfn "Swapper stats:"
    printfn "\tPositions: %d" (Map.count blockMap)
    printfn "\tGroups: %d" (Map.count groupToBlockIds)
    printfn "\tSwap instructions: %d" (List.length swaps)
    printfn "\tColor instructions: %d" (List.length postColorize)
    let reassignments =
        assignment
        |> Map.toSeq
        |> Seq.map (fun (posId, group) -> blockToGroup[posToBlockId[posId]] = group)
        |> Seq.filter (fun b -> not b)
        |> Seq.length
    printfn "\tReassignments: %d" reassignments
    printfn "\tSwap efficiency: %f" (
        float (List.length swaps) / float reassignments)
    (swaps @ postColorize, None)