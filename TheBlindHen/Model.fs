module Model

[<Struct>]
type Color = {
    r: byte
    g: byte
    b: byte
    a: byte
}

[<Struct>]
type Size = {
    width: int
    height: int
}

[<Struct>]
type Position = {
    x: int
    y: int
}

type Image = {
    size: Size
    pixels: Color[]
}

type Block(id, size, lowerLeft) =
    member val id: string = id with get

    // shape:
    member val size: Size = size with get
    member val lowerLeft: Position = lowerLeft with get

type SimpleBlock(id, size, lowerLeft, color) =
    inherit Block(id, size, lowerLeft)

    member val color: Color = color with get

type ComplexBlock(id, size, lowerLeft, children) =
    inherit Block(id, size, lowerLeft)

    member val children: Block[] = children with get

/// A canvas is made out of blocks
type Canvas = {
    maxTopId: int
    topBlocks: Map<string, Block>
}

let blankCanvas size = {
    maxTopId = 0
    topBlocks = Map.add "0" (
            SimpleBlock("0", size, {x = 0; y = 0}, {r = 255uy; g = 255uy; b = 255uy; a = 255uy})
        ) Map.empty
}
