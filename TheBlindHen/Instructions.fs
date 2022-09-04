module Instructions

open Model

[<RequireQualifiedAccess>]
type ISL =
    | LineCut of block: string * dir: Direction * offset: int
    | PointCut of block: string * position: Position
    | ColorBlock of block: string * color: Color
    | SwapBlocks of block1: string * block2: string
    | MergeBlocks of block1: string * block2: string

let deparse_instruction (isl: ISL) : string =
    let dir_to_str (dir: Direction) : string =
        match dir with
        | H -> "[Y]"
        | V -> "[X]"
    let pos_to_string (pos: Position) : string =
        sprintf "[%d,%d]" pos.x pos.y
    let block_to_string (block: string) : string =
        sprintf "[%s]" block
    let offset_to_str (offset: int) : string =
        sprintf "[%d]" offset
    match isl with
    | ISL.LineCut(block, dir, offset) ->
        sprintf "cut %s %s %s" (block_to_string block) (dir_to_str dir) (offset_to_str offset)
    | ISL.PointCut(block, position) ->
        sprintf "cut %s %s" (block_to_string block) (pos_to_string position)
    | ISL.ColorBlock(block, color) ->
        sprintf "color %s %s" (block_to_string block) (color.toString ())
    | ISL.SwapBlocks(block1, block2) ->
        sprintf "swap %s %s" (block_to_string block1) (block_to_string block2)
    | ISL.MergeBlocks(block1, block2) ->
        sprintf "merge %s %s" (block_to_string block1) (block_to_string block2)

let deparse (instructions: ISL list) =
    instructions
    |> List.map deparse_instruction
    |> String.concat "\n"

/// Returns the resulting canvas and the cost of the instruction
let simulate_step (canvas: Canvas) (isl: ISL) : (Canvas * int) =
    let canvasArea = float (canvas.size.width * canvas.size.height)
    match isl with
    | ISL.ColorBlock (blockId, color) ->
        let block = Map.find blockId canvas.topBlocks
        let new_block = SimpleBlock(block.id, block.size, block.lowerLeft, color)
        let cost = System.Math.Round (5.0 * canvasArea / float (block.size.width * block.size.height))
        { canvas with topBlocks = Map.add blockId new_block canvas.topBlocks }, int cost
    | ISL.PointCut (blockId, pos) ->
        let block = Map.find blockId canvas.topBlocks
        match block with
        | :? SimpleBlock as block ->
            // Slices of the block are numbered counterclockwise, starting from the bottom left
            let slice0 = SimpleBlock(blockId + ".0", {width = pos.x - block.lowerLeft.x; height = pos.y - block.lowerLeft.y}, block.lowerLeft, block.color)
            let slice1 = SimpleBlock(blockId + ".1", {width = block.size.width - (pos.x - block.lowerLeft.x); height = pos.y - block.lowerLeft.y}, {x = pos.x; y = block.lowerLeft.y}, block.color)
            let slice2 = SimpleBlock(blockId + ".2", {width = block.size.width - (pos.x - block.lowerLeft.x); height = block.size.height - (pos.y - block.lowerLeft.y)}, pos, block.color)
            let slice3 = SimpleBlock(blockId + ".3", {width = pos.x - block.lowerLeft.x; height = block.size.height - (pos.y - block.lowerLeft.y)}, {x = block.lowerLeft.x; y = pos.y}, block.color)
            let cost = System.Math.Round (10.0 * canvasArea / float (block.size.width * block.size.height))
            { canvas with
                topBlocks = canvas.topBlocks
                |> Map.remove blockId
                |> Map.add slice0.id slice0
                |> Map.add slice1.id slice1
                |> Map.add slice2.id slice2
                |> Map.add slice3.id slice3 }, int cost
        | _ -> failwith "Simulator: cutting a non-simple block is unimplemented"
    | ISL.LineCut (blockId, H, offset) ->
        let block = Map.find blockId canvas.topBlocks
        match block with
        | :? SimpleBlock as block ->
            let slice_top = SimpleBlock(blockId + ".1", {width = block.size.width; height = block.size.height - (offset - block.lowerLeft.y)}, {x = block.lowerLeft.x; y = offset}, block.color)
            let slice_bottom = SimpleBlock(blockId + ".0", {width = block.size.width; height = offset - block.lowerLeft.y}, block.lowerLeft, block.color)
            let cost = System.Math.Round (7.0 * canvasArea / float (block.size.width * block.size.height))
            { canvas with
                topBlocks = canvas.topBlocks
                |> Map.remove blockId
                |> Map.add slice_top.id slice_top
                |> Map.add slice_bottom.id slice_bottom }, int cost
        | _ -> failwith "Simulator: cutting a non-simple block is unimplemented"
    | ISL.LineCut (blockId, V, offset) ->
        let block = Map.find blockId canvas.topBlocks
        match block with
        | :? SimpleBlock as block ->
            let slice_left = SimpleBlock(blockId + ".0", {width = offset - block.lowerLeft.x; height = block.size.height}, block.lowerLeft, block.color)
            let slice_right = SimpleBlock(blockId + ".1", {width = block.size.width - (offset - block.lowerLeft.x); height = block.size.height}, {x = offset; y = block.lowerLeft.y}, block.color)
            let cost = System.Math.Round (7.0 * canvasArea / float (block.size.width * block.size.height))
            { canvas with
                topBlocks = canvas.topBlocks
                |> Map.remove blockId
                |> Map.add slice_left.id slice_left
                |> Map.add slice_right.id slice_right }, int cost
        | _ -> failwith "Simulator: cutting a non-simple block is unimplemented"
    | ISL.SwapBlocks (blockId1, blockId2) ->
        let block1 = Map.find blockId1 canvas.topBlocks
        let block2 = Map.find blockId2 canvas.topBlocks
        assert (block1.size = block2.size)
        if not (block1 :? SimpleBlock && block2 :? SimpleBlock) then failwith "Simulator: swapping non-simple blocks is unimplemented"
        let cost = System.Math.Round (5.0 * canvasArea / float (block1.size.width * block1.size.height))
        { canvas with
            topBlocks = canvas.topBlocks
            |> Map.remove blockId1
            |> Map.remove blockId2
            |> Map.add blockId1 (SimpleBlock(blockId1, block1.size, block2.lowerLeft, (block1 :?> SimpleBlock).color))
            |> Map.add blockId2 (SimpleBlock(blockId2, block2.size, block1.lowerLeft, (block2 :?> SimpleBlock).color)) }, int cost
    | _ -> failwith "Instruction not implemented"
/// Returns the resulting canvas and the cost of the program
let simulate (canvas: Canvas) (instructions: ISL list) : Canvas * int =
    instructions |> List.fold (fun (canvas, cost) isl ->
        let (canvas, islCost) = simulate_step canvas isl
        (canvas, cost + islCost)
    ) (canvas, 0)