open Model
open Loader
open GUI

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Usage TheBlindHen.exe [--ai] <target image>"
        1
    else
    let canvas = blankCanvas {width = 400; height = 400}
    if args[0] = "--ai" then
        /// AI dump to console
        let imgPath = args[1]
        let img = loadPNG imgPath
        let initBlock = canvas.topBlocks |> Map.find "0"
        let solution = [ AI.color_block_median (slice_whole_image img) initBlock ]
        let solution_str = Instructions.deparse solution
        printfn "AI solution for %s:\n%s" imgPath solution_str
        0
    else
        /// GUI
        let imgPath = args[0]
        showGui imgPath canvas |> ignore
        0
