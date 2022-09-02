module ISL

open Model

type ISL =
    | LineCut of block: string * dir: Direction * offset: int
    | PointCut of block: string * position: Position
    | Color of block: string * color: Color
    | Swap of block1: string * block2: string
    | Merge of block1: string * block2: string