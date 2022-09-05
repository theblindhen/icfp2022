module Model

[<Struct>]
type Color =
    { r: int
      g: int
      b: int
      a: int }
      member this.toString () =
        sprintf "[%d, %d, %d, %d]" (this.r) (this.g) (this.b) (this.a)
      member this.asInt () =
        (this.r <<< 24) ||| (this.g <<< 16) ||| (this.b <<< 8) ||| this.a

let colorOfInt (i: int) =
    { r = (i >>> 24) &&& 0xFF
      g = (i >>> 16) &&& 0xFF
      b = (i >>> 8) &&& 0xFF
      a = i &&& 0xFF }

[<Struct>]
type Size =
    { width: int
      height: int }
    member this.toString () =
        sprintf "[%d, %d]" this.width this.height

let area (size: Size) = size.width * size.height

[<Struct>]
type Position =
    { x: int
      y: int }
    member this.toString () =
        sprintf "[%d, %d]" this.x this.y

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

let sliceWholeImage (img: Image) =
    { size=img.size; offset={x=0; y=0}; _img=img }

let sliceImage (img: Image) (size: Size) (offset: Position) =
    { size=size; offset=offset; _img=img }

let subslice (img: ImageSlice) (size: Size) (offset: Position) =
    { size=size; offset={x=img.offset.x + offset.x; y=img.offset.y + offset.y}; _img=img._img }

let colorAtPosImg (img: Image) (pos: Position) : Color =
    img.pixels[pos.y * img.size.width + pos.x]

let inline colorAtPos (img: ImageSlice) (pos: Position) : Color =
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

type ImageBlock(id, size, lowerLeft, image) =
    inherit Block(id, size, lowerLeft)
    member val image: ImageSlice = image with get

/// A canvas is made out of blocks
type Canvas = {
    maxTopId: int
    size: Size
    topBlocks: Map<string, Block>
}

let blankCanvas size = {
    maxTopId = 0
    size = size
    topBlocks = Map.add "0" (
            SimpleBlock("0", size, {x = 0; y = 0}, {r = 255; g = 255; b = 255; a = 255})
        ) Map.empty
}

type GridInfo = {
    /// The size of each cell in the grid
    cellSize: Size
    /// The number of horizontal resp vertical cells in the grid
    dimensions: Size
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
    | :? ImageBlock as b ->
        for y in 0 .. (b.size.height - 1) do
            for x in 0 .. (b.size.width - 1) do
                let ty = y + offset.y + b.lowerLeft.y
                let tx = x + offset.x + b.lowerLeft.x
                image.pixels.[ty * image.size.width + tx] <- colorAtPos b.image {x=x; y=y}
    | _ -> failwith "Unknown block type"

let renderBlock (block: Block) =
    let image = { size = block.size; pixels = Array.zeroCreate (block.size.width * block.size.height) }
    renderBlockInto image {x=(-block.lowerLeft.x); y=(-block.lowerLeft.y)} block
    image

let renderCanvas (canvas: Canvas) =
    // TODO: Hard-coded canvas size to 400x400
    let image = { size = {width = 400; height = 400}
                  pixels = Array.zeroCreate (400 * 400) }
    canvas.topBlocks
    |> Map.iter (fun _ block -> renderBlockInto image {x=0; y=0} block)
    image