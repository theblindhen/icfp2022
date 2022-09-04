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

let getSolutionDir taskPath = $"{taskPath}.solutions/"

let writeSolutionIfBetter taskPath islSolution score =
    let solutionDir = getSolutionDir taskPath
    let dirinfo = IO.Directory.CreateDirectory solutionDir
    let solutionText = (deparse islSolution)
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
    | OptiTrace
    | SplitPoint of SplitPointSelector option
    | Repetitions of int
    | MergeAI of AISelector option
    | RandomSeed of int
    | [<MainCommand; ExactlyOnce; Last>] TaskPath of task:string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | GUI -> "Show the GUI"
            | AI _ -> "The AI to use"
            | OptiTrace _ -> "Apply opti-tracer to the generated solution. If no AI set, apply to the best solution on disk."
            | SplitPoint _ -> "The split point to use"
            | Repetitions _ -> "The number of repetitions"
            | TaskPath _ -> "The task png path to use"
            | Verbose _ -> "Print more information"
            | MergeAI _ -> "The AI to use inside the mergeMeta strategy, if chosen"
            | RandomSeed _ -> "The random seed to use"

and AISelector = OneLiner | QuadTree | Random | MCTS | EagerSwapper | AssignSwapper | MergeMeta
and SplitPointSelector = Midpoint | HighestDistance

let loadBestSolution taskPath =
    let solutionDir = getSolutionDir taskPath
    let dirinfo = IO.Directory.CreateDirectory solutionDir
    match bestCurrentSolution dirinfo with
    | None -> failwith "No solution found"
    | Some(best) ->
        let solutionFile = sprintf "%s%d.isl" solutionDir best
        parseSolutionFile solutionFile

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
    let solution, solverCost, solverSimilarity = AI.fastRandomSolver "0" {r=255;g=255;b=255;a=255} (sliceWholeImage targetImage) canvas
    solution, Some(solverCost, solverSimilarity)

let solverMCTS repetitions : Solver = fun targetImage canvas ->
    assert (Map.count canvas.topBlocks = 1) // MCTS does not support non-blank initial canvas
    let solution, solverCost, solverSimilarity = AI.mctsSolver repetitions (sliceWholeImage targetImage) canvas
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

let runAndScoreSolver taskPath (targetImage: Image) (initCanvas: Canvas) (solver: Solver) =
    let solution, solverComputedCostOpt = solver targetImage initCanvas
    let solutionCost, imageSimilarity = scoreSolution targetImage initCanvas solution
    match solverComputedCostOpt with
    | None -> ()
    | Some (solverCost, solverSimilarity) ->
        if solverCost <> solutionCost then
            printfn "WARNING: %s: Solver estimated cost %d, simulator estimated cost %d"
                taskPath solverCost solutionCost
        if solverSimilarity <> imageSimilarity then
            printfn "WARNING: %s: Solver estimated similarity %d, simulator estimated similarity %d"
                taskPath solverSimilarity imageSimilarity
    solution

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Arguments>(programName = "TheBlindHen.exe")
    let results = parser.Parse args
    let randomSeed = 
        match results.TryGetResult RandomSeed with
        | Some s -> s
        | None -> System.Random().Next()
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
        | MCTS -> 
            let repetitions = 
                match results.TryGetResult (Repetitions) with
                | None -> 1
                | Some n -> n
            (solverMCTS repetitions, "MCTS")
        | EagerSwapper -> (Swapper.eagerSwapper, "eager-swapper")
        | AssignSwapper -> (Swapper.assignSwapperSimple, "assign-swapper-simple")
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
        match results.GetResult (AI, None) with
        | None -> ((fun _ _ -> ([],None)), "no AI")
        | Some ai -> getSolver ai
    // Run the AI to get a solution
    let mutable solution =
        if solverName = "no AI" then
            printfn "Loading best solution"
            loadBestSolution taskPath
        else
            printfn "Task %s: Running %s solver" taskPath solverName
            runAndScoreSolver taskPath targetImage initCanvas solver
    // Optionally optimize the solution
    if results.Contains OptiTrace then
        solution <- OptiTrace.optimize targetImage initCanvas solution
    // Run the simulator to get the final score
    let (solutionScore, imageSimilarity) = scoreSolution targetImage initCanvas solution
    // Write it to disk if it was better
    writeSolutionIfBetter taskPath solution (solutionScore + imageSimilarity)
    // For the user: verbose output and/or GUI
    if results.Contains Verbose then
        printfn "Instructions generated:\n%s" (deparse solution)
    if results.Contains GUI then
        showGui initCanvas targetImage solution |> ignore
    0
