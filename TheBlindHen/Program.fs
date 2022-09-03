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
    let solution_canvas =
        match results.GetResult (AI) with
        | None -> canvas
        | Some (OneLiner) ->
            /// AI dump to console
            printfn "One-line solver on %s" taskPath
            let initBlock = canvas.topBlocks |> Map.find "0"
            let solution = [ AI.colorBlockMedian (sliceWholeImage task) initBlock ]
            printfn "Instructions:\n%s" (Instructions.deparse solution)
            let (solution_canvas, solution_cost) = Instructions.simulate canvas solution
            let solution_image = renderCanvas solution_canvas
            let image_distance = Util.imageDistance (sliceWholeImage task) (sliceWholeImage solution_image)
            printfn "Score %d (TODO: Only includes image distance)\nInstructions:\n%s" (image_distance) (Instructions.deparse solution)
            solution_canvas
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
            let (solution_canvas, solution_cost) = Instructions.simulate canvas solution
            let solution_image = renderCanvas solution_canvas
            let image_distance = Util.imageDistance (sliceWholeImage task) (sliceWholeImage solution_image)
            printfn "Score: %d" (solution_cost + image_distance)
            solution_canvas
    if results.Contains GUI then
        // GUI
        showGui task solution_canvas |> ignore
    0
