open Argu
open Model
open Loader
open GUI

type Arguments =
    | GUI
    | AI of AISelector option
    | SplitPoint of SplitPointSelector option
    | [<MainCommand; ExactlyOnce; Last>] Target of target:string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | GUI -> "Show the GUI"
            | AI _ -> "The AI to use"
            | SplitPoint _ -> "The split point to use"
            | Target _ -> "The target image to use"

and AISelector = OneLiner | QuadTree
and SplitPointSelector = Midpoint | HighestDistance

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Arguments>(programName = "TheBlindHen.exe")
    let results = parser.Parse args
    let taskPath = results.GetResult (Target)
    let task = loadPNG taskPath
    let canvas = blankCanvas {width = 400; height = 400}
    match results.GetResult (AI) with
    | None -> ()
    | Some (OneLiner) ->
        /// AI dump to console
        printfn "One-line solver on %s" taskPath
        let initBlock = canvas.topBlocks |> Map.find "0"
        let solution = [ AI.colorBlockMedian (sliceWholeImage task) initBlock ]
        let solution_canvas = Instructions.simulate canvas solution
        let solution_image = renderCanvas solution_canvas
        let image_distance = Util.imageDistance (sliceWholeImage task) (sliceWholeImage solution_image)
        printfn "Score %d (TODO: Only includes image distance)\nInstructions:\n%s" (image_distance) (Instructions.deparse solution)
    | Some (QuadTree) ->
        let splitpointSelector =
            match results.GetResult (SplitPoint) with
            | None -> AI.midpointCut
            | Some (Midpoint) -> AI.midpointCut
            | Some (HighestDistance) -> AI.highestDistanceCut
        printfn "Quadtree solver on %s" taskPath
        let solution =
            AI.quadtreeSolver splitpointSelector (sliceWholeImage task) canvas
        printfn "Instructions:\n%s" (Instructions.deparse solution)
    if results.Contains GUI then
        // GUI
        let imgPath = args[0]
        let target = loadPNG imgPath
        showGui target canvas |> ignore
    0
