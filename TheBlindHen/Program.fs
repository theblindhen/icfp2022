open Argu
open Model
open Loader
open GUI

open System

let bestCurrentSolution (dirinfo: IO.DirectoryInfo) =
    // For files named like "1234.isl", pick the one with the lowest number
    dirinfo.EnumerateFiles ()
    |> Seq.filter (fun f -> f.Extension = ".isl")
    |> Seq.map (fun f -> f.Name.Split('.')[0] |> int)
    |> Seq.sort
    |> Seq.tryHead

let getBestCurrentSolution solutionDir = 
    let dirinfo = IO.Directory.CreateDirectory solutionDir
    bestCurrentSolution dirinfo

let writeSolution taskPath islSolution score =
    let solutionDir = $"{taskPath}.solutions/"
    let solutionText = (Instructions.deparse islSolution)
    let dirinfo = IO.Directory.CreateDirectory solutionDir
    let solutionFile = sprintf "%s%d.isl" solutionDir score
    match bestCurrentSolution dirinfo with
    | None ->
        printfn "Found a new solution (score: %d). Writing solution to %s" score solutionFile
        IO.File.WriteAllText(solutionFile, solutionText)
    | Some(best) when score < best ->
        printfn "Found a better solution (score: %d -> %d). Writing solution to %s" best score solutionFile
        IO.File.WriteAllText(solutionFile, solutionText)
    | Some(best) ->
        printfn "A better solution already exists (score: %d). Not writing a new file. Solution (score: %d):" best score
        printfn "%s" solutionText

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
            printfn "One-line solver on %s" taskPath
            let initBlock = canvas.topBlocks |> Map.find "0"
            let solution = [ AI.colorBlockMedian (sliceWholeImage task) initBlock ]
            let (solution_canvas, solution_cost) = Instructions.simulate canvas solution
            let solution_image = renderCanvas solution_canvas
            let image_distance = Util.imageDistance (sliceWholeImage task) (sliceWholeImage solution_image)
            writeSolution taskPath solution (solution_cost + image_distance)
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
            let (solution_canvas, solution_cost) = Instructions.simulate canvas solution
            let solution_image = renderCanvas solution_canvas
            let image_distance = Util.imageDistance (sliceWholeImage task) (sliceWholeImage solution_image)
            writeSolution taskPath solution (solution_cost + image_distance)
            solution_canvas
    if results.Contains GUI then
        // GUI
        showGui task solution_canvas |> ignore
    0
