module Loader

open Model

open SixLabors.ImageSharp
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
                r = pixel.R
                g = pixel.G
                b = pixel.B
                a = pixel.A
            }
    {
        size = {width = img.Width; height = img.Height}
        pixels = pixels
    }
