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
        let taskPath = args[1]
        let task = loadPNG taskPath
        let initBlock = canvas.topBlocks |> Map.find "0"
        let solution = [ AI.color_block_median (slice_whole_image task) initBlock ]
        let solution_canvas = Instructions.simulate canvas solution
        let solution_image = renderCanvas solution_canvas
        let image_distance = Util.image_distance (slice_whole_image task) (slice_whole_image solution_image)
        printfn "AI on %s\nScore %d (TODO: Only includes image distance)\nInstructions:\n%s" taskPath (image_distance) (Instructions.deparse solution)
        0
    else
        /// GUI
        let taskPath = args[0]
        showGui taskPath canvas |> ignore
        0
