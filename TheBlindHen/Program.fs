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
        printfn "%s: Found a new solution (score: %d). Writing solution." taskPath score
        IO.File.WriteAllText(solutionFile, solutionText)
    | Some(best) when score < best ->
        printfn "%s: Found a better solution (score: %d -> %d). Writing solution" taskPath best score
        IO.File.WriteAllText(solutionFile, solutionText)
    | Some(best) when score = best ->
        printfn "%s: The best solution has the same score (score: %d). NOT writing solution" taskPath best
    | Some(best) ->
        printfn "%s: A better solution already exists (best score: %d, current score: %d). NOT writing solution" taskPath best score

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

and AISelector = OneLiner | QuadTree | MCTS
and SplitPointSelector = Midpoint | HighestDistance

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Arguments>(programName = "TheBlindHen.exe")
    let results = parser.Parse args
    let taskPath = results.GetResult (Target)
    let task = loadPNG taskPath
    let canvas = blankCanvas {width = 400; height = 400}
    let solution =
        match results.GetResult (AI) with
        | None -> []
        | Some (OneLiner) ->
            printfn "One-line solver on %s" taskPath
            let initBlock = canvas.topBlocks |> Map.find "0"
            let solution = [ AI.colorBlockMedian (sliceWholeImage task) initBlock ]
            let (solution_canvas, solution_cost) = Instructions.simulate canvas solution
            let solution_image = renderCanvas solution_canvas
            let imageSimilarity = Util.imageSimilarity (sliceWholeImage task) (sliceWholeImage solution_image)
            printfn "Similarity: %d" imageSimilarity
            writeSolution taskPath solution (solution_cost + imageSimilarity)
            solution
        | Some (QuadTree) ->
            let splitpointSelector =
                match results.GetResult (SplitPoint) with
                | None -> AI.midpointCut
                | Some (Midpoint) -> AI.midpointCut
                | Some (HighestDistance) -> AI.highestDistanceCut
            printfn "%s: Running quadtree solver" taskPath
            let solution, solverCost, solverSimilarity =
                AI.quadtreeSolver splitpointSelector (sliceWholeImage task) canvas
            let (solution_canvas, solution_cost) = Instructions.simulate canvas solution
            let solution_image = renderCanvas solution_canvas
            let imageSimilarity = Util.imageSimilarity (sliceWholeImage task) (sliceWholeImage solution_image)
            writeSolution taskPath solution (solution_cost + imageSimilarity)
            if solverCost <> solution_cost then
                printfn "WARNING: %s: Solver estimated cost %d, simulator estimated cost %d"
                    taskPath solverCost solution_cost
            if solverSimilarity <> imageSimilarity then
                printfn "WARNING: %s: Solver estimated similarity %d, simulator estimated similarity %d"
                    taskPath solverSimilarity imageSimilarity
            solution
        | Some (MCTS) ->
            printfn "%s: Running MCTS solver" taskPath
            let solution, solverCost, solverSimilarity =
                AI.mctsSolver (sliceWholeImage task) canvas
            let (solution_canvas, solution_cost) = Instructions.simulate canvas solution
            let solution_image = renderCanvas solution_canvas
            let imageSimilarity = Util.imageSimilarity (sliceWholeImage task) (sliceWholeImage solution_image)
            writeSolution taskPath solution (solution_cost + imageSimilarity)
            if solverCost <> solution_cost then
                printfn "WARNING: %s: Solver estimated cost %d, simulator estimated cost %d"
                    taskPath solverCost solution_cost
            if solverSimilarity <> imageSimilarity then
                printfn "WARNING: %s: Solver estimated similarity %d, simulator estimated similarity %d"
                    taskPath solverSimilarity imageSimilarity
            solution
    if results.Contains GUI then
        // GUI
        showGui task solution |> ignore
    0
