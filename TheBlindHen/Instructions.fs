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
        | H -> "[X]"
        | V -> "[Y]"
    let pos_to_string (pos: Position) : string =
        sprintf "[%d,%d]" pos.x pos.y
    let color_to_string (color: Color) : string =
        sprintf "[%d,%d,%d,%d]" color.r color.g color.b color.a
    let block_to_string (block: string) : string =
        sprintf "[%s]" block
    match isl with
    | ISL.LineCut(block, dir, offset) ->
        sprintf "cut %s %s %d" (block_to_string block) (dir_to_str dir) offset
    | ISL.PointCut(block, position) ->
        sprintf "cut %s %s" (block_to_string block) (pos_to_string position)
    | ISL.ColorBlock(block, color) ->
        sprintf "color %s %s" (block_to_string block) (color_to_string color)
    | ISL.SwapBlocks(block1, block2) ->
        sprintf "swap %s %s" (block_to_string block1) (block_to_string block2)
    | ISL.MergeBlocks(block1, block2) ->
        sprintf "merge %s %s" (block_to_string block1) (block_to_string block2)

let deparse (instructions: ISL list) =
    instructions
    |> List.map deparse_instruction
    |> String.concat "\n"