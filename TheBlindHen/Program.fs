open Model
open Loader
open GUI

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Usage TheBlindHen.exe [--ai|--quadtree] <target image>"
        1
    else
    let canvas = blankCanvas {width = 400; height = 400}
    if args[0] = "--ai" then
        /// AI dump to console
        let taskPath = args[1]
        let task = loadPNG taskPath
        let initBlock = canvas.topBlocks |> Map.find "0"
        let solution = [ AI.colorBlockMedian (sliceWholeImage task) initBlock ]
        let solution_canvas = Instructions.simulate canvas solution
        let solution_image = renderCanvas solution_canvas
        let image_distance = Util.imageDistance (sliceWholeImage task) (sliceWholeImage solution_image)
        printfn "One-line solver on %s\nScore %d (TODO: Only includes image distance)\nInstructions:\n%s" taskPath (image_distance) (Instructions.deparse solution)
        0
    else if args[0] = "--quadtree" then
        let taskPath = args[1]
        let task = loadPNG taskPath
        let solution =
            AI.quadtreeSolver
                //AI.midpointCut
                AI.highestDistanceCut
                (sliceWholeImage task) canvas
        printfn "Quadtree on %s\nInstructions:\n%s" taskPath (Instructions.deparse solution)
        0
    else
        /// GUI
        let imgPath = args[0]
        let target = loadPNG imgPath
        showGui target canvas |> ignore
        0
