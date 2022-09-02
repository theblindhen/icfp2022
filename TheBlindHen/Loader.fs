module Loader

open Model

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.PixelFormats

let loadPNG (imgFilePath: string) : Model.Image =
    let img = Image.Load<Rgba32> imgFilePath
    let pixelArray : Rgba32 array = Array.zeroCreate (img.Width * img.Height)
    img.CopyPixelDataTo(pixelArray)
    let pixels = pixelArray |> Array.map (fun p -> {r = p.R; g = p.G; b = p.B; a = p.A})
    {
        size = {width = img.Width; height = img.Height}
        pixels = pixels
    }
