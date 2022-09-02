module Loader

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.PixelFormats

let loadPNG (imgFilePath: string) : (int * int * Rgba32 array) =
    let img = Image.Load<Rgba32> imgFilePath
    let pixelArray : Rgba32 array = Array.zeroCreate (img.Width * img.Height)
    img.CopyPixelDataTo(pixelArray)
    (img.Width, img.Height, pixelArray)