open Model
open Loader
open GUI

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Usage TheBlindHen.exe [--oneline|--quadtree] <target image>"
        1
    else
    let canvas = blankCanvas {width = 400; height = 400}
    if args[0] = "--oneline" then
        /// AI dump to console
        let taskPath = args[1]
        printfn "One-line solver on %s" taskPath
        let task = loadPNG taskPath
        let initBlock = canvas.topBlocks |> Map.find "0"
        let solution = [ AI.colorBlockMedian (sliceWholeImage task) initBlock ]
        printfn "Instructions:\n%s" (Instructions.deparse solution)
        let (solution_canvas, solution_cost) = Instructions.simulate canvas solution
        let solution_image = renderCanvas solution_canvas
        let image_distance = Util.imageDistance (sliceWholeImage task) (sliceWholeImage solution_image)
        printfn "Score: %d" (solution_cost + image_distance)
        0
    else if args[0] = "--quadtree" then
        let taskPath = args[1]
        printfn "Quadtree solver on %s" taskPath
        let task = loadPNG taskPath
        let solution =
            AI.quadtreeSolver
                //AI.midpointCut
                AI.highestDistanceCut
                (sliceWholeImage task) canvas
        printfn "Instructions:\n%s" (Instructions.deparse solution)
        let (solution_canvas, solution_cost) = Instructions.simulate canvas solution
        let solution_image = renderCanvas solution_canvas
        let image_distance = Util.imageDistance (sliceWholeImage task) (sliceWholeImage solution_image)
        printfn "Score: %d" (solution_cost + image_distance)
        0
    else
        /// GUI
        let imgPath = args[0]
        let target = loadPNG imgPath
        showGui target canvas |> ignore
        0
