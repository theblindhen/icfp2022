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
    | [<MainCommand; ExactlyOnce; Last>] TaskPath of task:string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | GUI -> "Show the GUI"
            | AI _ -> "The AI to use"
            | SplitPoint _ -> "The split point to use"
            | TaskPath _ -> "The task png path to use"

and AISelector = OneLiner | QuadTree | MCTS | EagerSwapper
and SplitPointSelector = Midpoint | HighestDistance

type Solver = Image -> Canvas -> Instructions.ISL list * (int * int) option

// TODO: These should take a task, not just a taskImage
let solverOneLiner : Solver = fun targetImage canvas ->
    assert (Map.count canvas.topBlocks = 1) // oneLiner does not support non-blank initial canvas
    let startingBlock = canvas.topBlocks |> Map.find "0"
    ([ AI.colorBlockMedian (sliceWholeImage targetImage) startingBlock ], None)

let solverQuadTree : AI.SplitPointSelector -> Solver = fun splitpointSelector targetImage canvas ->
    assert (Map.count canvas.topBlocks = 1) // quadTree does not support non-blank initial canvas
    let solution, solverCost, solverSimilarity = AI.quadtreeSolver splitpointSelector (sliceWholeImage targetImage) canvas
    solution, Some(solverCost, solverSimilarity)

let solverMCTS : Solver = fun targetImage canvas ->
    assert (Map.count canvas.topBlocks = 1) // MCTS does not support non-blank initial canvas
    let solution, solverCost, solverSimilarity = AI.mctsSolver (sliceWholeImage targetImage) canvas
    solution, Some(solverCost, solverSimilarity)

let solverEagerSwapper : Solver = fun targetImage canvas ->
    Swapper.eagerSwapper targetImage canvas

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Arguments>(programName = "TheBlindHen.exe")
    let results = parser.Parse args
    let taskPath = results.GetResult (TaskPath)
    let targetImage = loadPNG taskPath
    let initCanvas =
        let taskFileInfo = System.IO.FileInfo (taskPath)
        let taskInitPath = taskFileInfo.DirectoryName + "/" + taskFileInfo.Name.Split('.')[0] + ".initial.json"
        if IO.File.Exists taskInitPath then
            loadSimpleCanvasJson taskInitPath
        else 
            printfn "Canvas was blank"
            blankCanvas {width = 400; height = 400}
    let (solver, solverName) = 
        match results.GetResult (AI) with
        | None -> ((fun _ _ -> ([],None)), "no AI")
        | Some (OneLiner) -> (solverOneLiner, "one-line")
        | Some (QuadTree) ->
            let splitpointSelector =
                match results.GetResult (SplitPoint) with
                | None -> AI.midpointCut
                | Some (Midpoint) -> AI.midpointCut
                | Some (HighestDistance) -> AI.highestDistanceCut
            (solverQuadTree splitpointSelector, "quad-tree")
        | Some (MCTS) -> (solverMCTS, "MCTS")
        | Some EagerSwapper -> (solverEagerSwapper, "eager-swapper")
    printfn "Task %s: Running %s solver" taskPath solverName
    let solution, goodnessOpt = solver targetImage initCanvas
    let (solutionCanvas, solutionCost) = Instructions.simulate initCanvas solution
    let solutionImage = renderCanvas solutionCanvas
    let imageSimilarity = Util.imageSimilarity (sliceWholeImage targetImage) (sliceWholeImage solutionImage)
    writeSolution taskPath solution (solutionCost + imageSimilarity)
    match goodnessOpt with
    | None -> ()
    | Some (solverCost, solverSimilarity) ->
        if solverCost <> solutionCost then
            printfn "WARNING: %s: Solver estimated cost %d, simulator estimated cost %d"
                taskPath solverCost solutionCost
        if solverSimilarity <> imageSimilarity then
            printfn "WARNING: %s: Solver estimated similarity %d, simulator estimated similarity %d"
                taskPath solverSimilarity imageSimilarity
    if results.Contains GUI then
        // GUI
        showGui initCanvas targetImage solution |> ignore
    0
