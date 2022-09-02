module Instructions

open Model

[<RequireQualifiedAccess>]
type ISL =
    | LineCut of block: string * dir: Direction * offset: int
    | PointCut of block: string * position: Position
    | ColorBlock of block: string * color: Color
    | SwapBlocks of block1: string * block2: string
    | MergeBlocks of block1: string * block2: string