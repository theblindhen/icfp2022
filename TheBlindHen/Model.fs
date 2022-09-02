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

let area (size: Size) = size.width * size.height

[<Struct>]
type Position = {
    x: int
    y: int
}

type Image = {
    size: Size
    /// The pixels of the image, in row-major order (first row first, then
    /// second row, etc.). The y-axis points upwards, so the first pixel in the
    /// array is the bottom-left pixel of the image.
    pixels: Color[]
}

[<Struct>]
type ImageSlice = {
    /// Size of the slice inside the original image
    size: Size
    /// Offset of the lower-left hand of the slice inside the original image
    offset: Position
    /// Original image
    _img: Image
}

let slice_whole_image (img: Image) =
    { size=img.size; offset={x=0; y=0}; _img=img }

let slice_image (img: Image) (size: Size) (offset: Position) =
    { size=size; offset=offset; _img=img }

let color_at_pos_img (img: Image) (pos: Position) : Color =
    img.pixels[pos.y * img.size.width + pos.x]

let color_at_pos (img: ImageSlice) (pos: Position) : Color =
    img._img.pixels[(img.offset.y + pos.y) * img._img.size.width + (img.offset.x + pos.x)]



type Block(id, size, lowerLeft) =
    member val id: string = id with get
    member val size: Size = size with get
    /// Lower-left corner of the block, in absolute coordinates from the canvas
    /// lower-left corner.
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

type Direction = H | V

let rec renderBlockInto image offset (block: Block) =
    match block with
    | :? SimpleBlock as b ->
        for y in 0 .. (b.size.height - 1) do
            for x in 0 .. (b.size.width - 1) do
                let y = y + offset.y + b.lowerLeft.y
                let x = x + offset.x + b.lowerLeft.x
                image.pixels.[y * image.size.width + x] <- b.color
    | :? ComplexBlock as b ->
        b.children |> Array.iter (renderBlockInto image offset)
    | _ -> failwith "Unknown block type"

let renderBlock (block: Block) =
    let image = { size = block.size; pixels = Array.zeroCreate (block.size.width * block.size.height) }
    renderBlockInto image {x=(-block.lowerLeft.x); y=(-block.lowerLeft.y)} block
    image
