module Loader

open Model

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Formats.Png
open SixLabors.ImageSharp.Formats.Bmp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.PixelFormats

let loadPNG (imgFilePath: string) : Model.Image =
    let img = Image.Load<Rgba32> imgFilePath
    let pixelArray : Rgba32 array = Array.zeroCreate (img.Width * img.Height)
    img.CopyPixelDataTo(pixelArray)
    let pixels : Model.Color array = Array.zeroCreate (img.Width * img.Height)
    // Turn the y axis upside down
    for y in 0 .. img.Height - 1 do
        for x in 0 .. img.Width - 1 do
            let pixel = pixelArray.[(img.Height - y - 1) * img.Width + x]
            pixels.[y * img.Width + x] <- {
                r = int pixel.R
                g = int pixel.G
                b = int pixel.B
                a = int pixel.A
            }
    {
        size = {width = img.Width; height = img.Height}
        pixels = pixels
    }

let toImageSharp (img : Model.Image) : Image<Rgba32> =
    let pixelArray : Rgba32 array = Array.zeroCreate (img.size.width * img.size.height)
    for y in 0 .. img.size.height - 1 do
        for x in 0 .. img.size.width - 1 do
            let pixel = img.pixels.[(img.size.height - y - 1) * img.size.width + x]
            pixelArray.[y * img.size.width + x] <- Rgba32(byte pixel.r, byte pixel.g, byte pixel.b, byte pixel.a)
    Image.LoadPixelData(pixelArray, img.size.width, img.size.height)

let resize (size: Model.Size) (img: Image<Rgba32>) =
    img.Mutate(fun x -> x.Resize(size.width, size.height, KnownResamplers.NearestNeighbor) |> ignore)

let toPngStream (img: Image<Rgba32>) : MemoryStream =
    let imgStream = new MemoryStream()
    img.Save(imgStream, PngFormat.Instance)
    imgStream.Seek(0,  SeekOrigin.Begin) |> ignore
    imgStream


open System.Text.Json

type JsonSimpleBlock = {
    blockId: string
    bottomLeft: int list // Always 2 entries
    topRight: int list   // Always 2 entries
    color: int list      // Always 4 entries
}

type JsonSimpleCanvas = {
    width: int
    height: int
    blocks: JsonSimpleBlock list 
}

let loadSimpleCanvasJson (jsonFilePath: string) =
    let json = File.ReadAllText(jsonFilePath)
    let jsonCanvas = JsonSerializer.Deserialize<JsonSimpleCanvas>(json)
    let blocks: Block list = jsonCanvas.blocks |> List.map (fun b ->
        SimpleBlock(
            b.blockId,
            { width = b.topRight.[0] - b.bottomLeft.[0]
              height = b.topRight.[1] - b.bottomLeft.[1] },
            { x = b.bottomLeft.[0]
              y = b.bottomLeft.[1] },
            { r = b.color.[0]
              g = b.color.[1]
              b = b.color.[2]
              a = b.color.[3] })
        )
    { maxTopId = blocks.Length - 1
      size = { width = jsonCanvas.width
               height = jsonCanvas.height }
      topBlocks =
        blocks
        |> List.map (fun b -> (b.id, b))
        |> Map.ofList 
    }