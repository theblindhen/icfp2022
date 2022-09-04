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

type Solver = Image -> Canvas -> ISL list * (int * int) option

[<RequireQualifiedAccess>]
type ISLOps =
    | LineCut
    | PointCut
    | ColorBlock
    | SwapBlocks
    | MergeBlocks

type Costs = {
    lineCut: float
    pointCut: float
    color: float
    swap: float
    merge: float
}
let baseCost = { 
    lineCut = 7.0;
    pointCut = 10.0;
    color = 5.0;
    swap = 3.0;
    merge = 1.0
}

let islBaseCost op =
    match op with
    | ISLOps.LineCut -> 7.0
    | ISLOps.PointCut -> 10.0
    | ISLOps.ColorBlock -> 5.0
    | ISLOps.SwapBlocks -> 3.0
    | ISLOps.MergeBlocks -> 1.0

let sizeMultiplier (canvas: Canvas) (operationSize: Size) =
    float (canvas.size.width * canvas.size.height) / float (operationSize.width * operationSize.height) 

let islCost (canvas: Canvas) (islOp: ISLOps) (operationSize: Size) =
    let baseCost = islBaseCost islOp
    let sizeMultiplier = sizeMultiplier canvas operationSize
    int (System.Math.Round (baseCost * sizeMultiplier))

/// Returns the resulting canvas and the cost of the instruction
let simulate_step (canvas: Canvas) (isl: ISL) : (Canvas * int) =
    match isl with
    | ISL.ColorBlock (blockId, color) ->
        let block = canvas.topBlocks[blockId]
        let new_block = SimpleBlock(block.id, block.size, block.lowerLeft, color)
        let cost = islCost canvas ISLOps.ColorBlock block.size
        { canvas with topBlocks = Map.add blockId new_block canvas.topBlocks }, cost
    | ISL.PointCut (blockId, pos) ->
        let block = Map.find blockId canvas.topBlocks
        match block with
        | :? SimpleBlock as block ->
            // Slices of the block are numbered counterclockwise, starting from the bottom left
            let slice0 = SimpleBlock(blockId + ".0", {width = pos.x - block.lowerLeft.x; height = pos.y - block.lowerLeft.y}, block.lowerLeft, block.color)
            let slice1 = SimpleBlock(blockId + ".1", {width = block.size.width - (pos.x - block.lowerLeft.x); height = pos.y - block.lowerLeft.y}, {x = pos.x; y = block.lowerLeft.y}, block.color)
            let slice2 = SimpleBlock(blockId + ".2", {width = block.size.width - (pos.x - block.lowerLeft.x); height = block.size.height - (pos.y - block.lowerLeft.y)}, pos, block.color)
            let slice3 = SimpleBlock(blockId + ".3", {width = pos.x - block.lowerLeft.x; height = block.size.height - (pos.y - block.lowerLeft.y)}, {x = block.lowerLeft.x; y = pos.y}, block.color)
            let cost = islCost canvas ISLOps.PointCut block.size
            { canvas with
                topBlocks = canvas.topBlocks
                |> Map.remove blockId
                |> Map.add slice0.id slice0
                |> Map.add slice1.id slice1
                |> Map.add slice2.id slice2
                |> Map.add slice3.id slice3 }, cost
        | _ -> failwith "Simulator: cutting a non-simple block is unimplemented"
    | ISL.LineCut (blockId, H, offset) ->
        let block = Map.find blockId canvas.topBlocks
        match block with
        | :? SimpleBlock as block ->
            let slice_top = SimpleBlock(blockId + ".1", {width = block.size.width; height = block.size.height - (offset - block.lowerLeft.y)}, {x = block.lowerLeft.x; y = offset}, block.color)
            let slice_bottom = SimpleBlock(blockId + ".0", {width = block.size.width; height = offset - block.lowerLeft.y}, block.lowerLeft, block.color)
            let cost = islCost canvas ISLOps.LineCut block.size
            { canvas with
                topBlocks = canvas.topBlocks
                |> Map.remove blockId
                |> Map.add slice_top.id slice_top
                |> Map.add slice_bottom.id slice_bottom }, cost
        | _ -> failwith "Simulator: cutting a non-simple block is unimplemented"
    | ISL.LineCut (blockId, V, offset) ->
        let block = Map.find blockId canvas.topBlocks
        match block with
        | :? SimpleBlock as block ->
            let slice_left = SimpleBlock(blockId + ".0", {width = offset - block.lowerLeft.x; height = block.size.height}, block.lowerLeft, block.color)
            let slice_right = SimpleBlock(blockId + ".1", {width = block.size.width - (offset - block.lowerLeft.x); height = block.size.height}, {x = offset; y = block.lowerLeft.y}, block.color)
            let cost = islCost canvas ISLOps.LineCut block.size
            { canvas with
                topBlocks = canvas.topBlocks
                |> Map.remove blockId
                |> Map.add slice_left.id slice_left
                |> Map.add slice_right.id slice_right }, cost
        | _ -> failwith "Simulator: cutting a non-simple block is unimplemented"
    | ISL.SwapBlocks (blockId1, blockId2) ->
        let block1 = Map.find blockId1 canvas.topBlocks
        let block2 = Map.find blockId2 canvas.topBlocks
        assert (block1.size = block2.size)
        let cost = islCost canvas ISLOps.SwapBlocks block1.size
        let blockWithLowerLeft (block: Block) lowerLeft : Block =
            match block with
            | :? SimpleBlock as block -> SimpleBlock(block.id, block.size, lowerLeft, block.color)
            | :? ComplexBlock as block -> ComplexBlock(block.id, block.size, lowerLeft, block.children)
            | :? ImageBlock as block -> ImageBlock(block.id, block.size, lowerLeft, block.image)
            | _ -> failwith "Unknown type of block"
        { canvas with
            topBlocks = canvas.topBlocks
            |> Map.add blockId1 (blockWithLowerLeft block1 block2.lowerLeft)
            |> Map.add blockId2 (blockWithLowerLeft block2 block1.lowerLeft) }, cost
    | ISL.MergeBlocks (blockId1, blockId2) ->
        let maxTopId = canvas.maxTopId
        let block1 = Map.find blockId1 canvas.topBlocks
        let block2 = Map.find blockId2 canvas.topBlocks
        // Assert that block1 and block2 are adjacent
        let lowerLeft, size =
            if block1.lowerLeft.x = block2.lowerLeft.x then
                // Blocks are on the same vertical line
                assert (block1.lowerLeft.y = block2.lowerLeft.y + block2.size.height || block2.lowerLeft.y = block1.lowerLeft.y + block1.size.height)
                assert (block1.size.width = block2.size.width)
                ({ x = block1.lowerLeft.x;
                   y = System.Math.Min(block1.lowerLeft.y, block2.lowerLeft.y) },
                 { width = block1.size.width;
                   height = block1.size.height + block2.size.height })
            elif block1.lowerLeft.y = block2.lowerLeft.y then
                // Blocks are on the same horizontal line
                assert (block1.lowerLeft.x = block2.lowerLeft.x + block2.size.width || block2.lowerLeft.x = block1.lowerLeft.x + block1.size.width)
                assert (block1.size.height = block2.size.height)
                ({ x = System.Math.Min(block1.lowerLeft.x, block2.lowerLeft.x);
                   y = block1.lowerLeft.y },
                 { width = block1.size.width + block2.size.width;
                   height = block1.size.height })
            else failwith "Simulator: merging non-adjacent blocks"
        // Create a complex block for the purpose of rendering it
        let complexBlock = ComplexBlock("temp", size, lowerLeft, [|block1; block2|])
        let imageBlock = ImageBlock(string (maxTopId + 1), size, lowerLeft, sliceWholeImage(renderBlock complexBlock))
        let cost = islCost canvas ISLOps.MergeBlocks size
        { canvas with
            maxTopId = maxTopId + 1;
            topBlocks = canvas.topBlocks
            |> Map.remove blockId1
            |> Map.remove blockId2
            |> Map.add imageBlock.id imageBlock }, cost

/// Returns the resulting canvas and the cost of the program
let simulate (canvas: Canvas) (instructions: ISL list) : Canvas * int =
    instructions |> List.fold (fun (canvas, cost) isl ->
        let (canvas, islCost) = simulate_step canvas isl
        (canvas, cost + islCost)
    ) (canvas, 0)

let scoreSolution (targetImage: Image) (initCanvas: Canvas) (solution: ISL list) =
    let (solutionCanvas, solutionCost) = simulate initCanvas solution
    let solutionImage = renderCanvas solutionCanvas
    let imageSimilarity = Util.imageSimilarity (sliceWholeImage targetImage) (sliceWholeImage solutionImage)
    (solutionCost, imageSimilarity)