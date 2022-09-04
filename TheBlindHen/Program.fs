open Argu
open Model
open Loader
open GUI
open Instructions

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
    let solutionText = (deparse islSolution)
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
    | [<AltCommandLine("-v")>] Verbose
    | AI of AISelector option
    | SplitPoint of SplitPointSelector option
    | Repetitions of int
    | MergeAI of AISelector option
    | [<MainCommand; ExactlyOnce; Last>] TaskPath of task:string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | GUI -> "Show the GUI"
            | AI _ -> "The AI to use"
            | SplitPoint _ -> "The split point to use"
            | Repetitions _ -> "The number of repetitions"
            | TaskPath _ -> "The task png path to use"
            | Verbose _ -> "Print more information"
            | MergeAI _ -> "The AI to use inside the mergeMeta strategy, if chosen"

and AISelector = OneLiner | QuadTree | Random | MCTS | EagerSwapper | AssignSwapper | MergeMeta
and SplitPointSelector = Midpoint | HighestDistance

let solverOneLiner : Solver = fun targetImage canvas ->
    if Map.count canvas.topBlocks > 1 then
        ([], None) // oneLiner does not support non-blank initial canvas
    else
    let startingBlock = canvas.topBlocks |> Map.find "0" :?> SimpleBlock
    (AI.colorBlockMedianIfBeneficial (sliceWholeImage targetImage) canvas startingBlock, None)

let solverQuadTree : AI.SplitPointSelector -> Solver = fun splitpointSelector targetImage canvas ->
    assert (Map.count canvas.topBlocks = 1) // quadTree does not support non-blank initial canvas
    let solution, solverCost, solverSimilarity = AI.quadtreeSolver splitpointSelector (sliceWholeImage targetImage) canvas
    solution, Some(solverCost, solverSimilarity)

let solverRandom : Solver = fun targetImage canvas ->
    assert (Map.count canvas.topBlocks = 1) // MCTS does not support non-blank initial canvas
    let solution, solverCost, solverSimilarity = AI.fastRandomSolver (sliceWholeImage targetImage) canvas
    solution, Some(solverCost, solverSimilarity)

let solverMCTS : Solver = fun targetImage canvas ->
    assert (Map.count canvas.topBlocks = 1) // MCTS does not support non-blank initial canvas
    let solution, solverCost, solverSimilarity = AI.mctsSolver (sliceWholeImage targetImage) canvas
    solution, Some(solverCost, solverSimilarity)

let penalty x =
    match x with
    | None -> 0
    | Some (cost, similarity) -> cost + similarity

let rerunSolver n (solver: Solver) img canvas =
    let mutable bestSolution, lowestCosts = solver img canvas
    for _ in 1..n do
        let solution, costs = solver img canvas
        if penalty costs < penalty lowestCosts then
            bestSolution <- solution
            lowestCosts <- costs
    bestSolution, lowestCosts

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Arguments>(programName = "TheBlindHen.exe")
    let results = parser.Parse args
    let randomSeed = System.Random().Next()
    printfn "Random seed: %d" randomSeed
    Rng.rng <- System.Random(randomSeed) // TODO: add a command-line option for overriding this
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
    let rec getSolver = function
        | OneLiner -> (solverOneLiner, "one-line")
        | QuadTree ->
            let splitpointSelector =
                match results.GetResult (SplitPoint) with
                | None -> AI.midpointCut
                | Some (Midpoint) -> AI.midpointCut
                | Some (HighestDistance) -> AI.highestDistanceCut
            (solverQuadTree splitpointSelector, "quad-tree")
        | MCTS -> (solverMCTS, "MCTS")
        | EagerSwapper -> (Swapper.eagerSwapper, "eager-swapper")
        | AssignSwapper -> (Swapper.assignSwapper, "assign-swapper")
        | Random ->
            let repetitions =
                match results.TryGetResult (Repetitions) with
                | None -> 1
                | Some n -> n
            (rerunSolver repetitions solverRandom, "random")
        | MergeMeta ->
            match results.GetResult (MergeAI) with
            | None -> failwith "MergeMeta requires a MergeAI argument"
            | Some MergeMeta -> failwith "Recursive MergeMeta not yet supported"
            | Some mergeAI ->
                let innerSolver, innerSolverName = getSolver mergeAI
                (Merger.mergeMetaSolver innerSolver, $"merge-meta({innerSolverName})")
    let (solver, solverName) =
        match results.GetResult (AI) with
        | None -> ((fun _ _ -> ([],None)), "no AI")
        | Some ai -> getSolver ai
    printfn "Task %s: Running %s solver" taskPath solverName
    let solution, goodnessOpt = solver targetImage initCanvas
    let (solutionCanvas, solutionCost) = simulate initCanvas solution
    let solutionImage = renderCanvas solutionCanvas
    let imageSimilarity = Util.imageSimilarity (sliceWholeImage targetImage) (sliceWholeImage solutionImage)
    if results.Contains Verbose then
        printfn "Instructions generated:\n%s" (deparse solution)
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
