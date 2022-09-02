open Model
open Loader
open GUI

[<EntryPoint>]
let main args =
    let canvas = blankCanvas {width = 400; height = 400}
    if args.Length < 1 then
        printfn "Specify path to target image as first argument"
        1
    else
        let imgPath = args[0]
        let target = loadPNG imgPath
        showGui target canvas |> ignore
        0
