module Swapper

open Model
open Instructions

let assertAllBlockSameSize canvas =
    let b0 = Map.find "0" canvas.topBlocks
    for b in Map.values canvas.topBlocks do
        assert (b.size = b0.size)

let eagerSwapper (targetImage: Image) (canvas: Canvas) =
    assertAllBlockSameSize canvas
    // Since swapping takes the id's with it, we create a permanent numbering of
    // all the locations on the canvas
    let (_, blockMap, positionMap) =
        Map.values canvas.topBlocks
        |> List.ofSeq
        |> List.sortBy (fun b -> b.lowerLeft.y * canvas.size.width + b.lowerLeft.x)
        |> Seq.fold (fun (n, blockMap, positionMap) block ->
                (n+1, Map.add n (block :?> SimpleBlock) blockMap, Map.add block.lowerLeft n positionMap)
            ) (0, Map.empty, Map.empty)
    let chooseGreedySwap (blockMap: Map<int, SimpleBlock>) posId =
        let curBlock = Map.find posId blockMap
        let curTarget = sliceImage targetImage curBlock.size curBlock.lowerLeft
        let distToCur posId =
            Util.singleColorDistance ((Map.find posId blockMap).color) curTarget
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
                let block = Map.find posId blockMap
                let swap = Map.find swapPosId blockMap
                printfn "Swapping block [%s] at %d (%s) with [%s] at %d (%s)" block.id posId (block.color.toString()) swap.id swapPosId (swap.color.toString())
                let newBlockMap =
                    blockMap
                    |> Map.add swapPosId (SimpleBlock(block.id, block.size, swap.lowerLeft, block.color))
                    |> Map.add posId (SimpleBlock(swap.id, swap.size, block.lowerLeft, swap.color))
                let newSwaps = ISL.SwapBlocks(block.id, swap.id) :: swaps
                (newBlockMap, newSwaps))
            (blockMap, [])
        |> snd
        |> List.rev
    (instructions, None)