open Loader

[<EntryPoint>]
let main args =
    let imgPath = args[0]
    let (width, height, pixelArray) = loadPNG imgPath
    printfn "loaded target image (image path: %s, width: %d, height: %d)" imgPath width height
    0
