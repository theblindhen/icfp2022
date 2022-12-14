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

let prepareTask taskPath =
    let targetImage = loadPNG taskPath
    let initCanvas =
        let taskFileInfo = System.IO.FileInfo (taskPath)
        let taskInitPath = taskFileInfo.DirectoryName + "/" + taskFileInfo.Name.Split('.')[0] + ".initial.json"
        if IO.File.Exists taskInitPath then
            loadSimpleCanvasJson taskInitPath
        else 
            // printfn "Canvas was blank"
            blankCanvas {width = 400; height = 400}
    (targetImage, initCanvas)

let loadBestSolution taskPath =
    let solutionDir = getSolutionDir taskPath
    let dirinfo = IO.Directory.CreateDirectory solutionDir
    match bestCurrentSolution dirinfo with
    | None -> failwith "No solution found"
    | Some(best) ->
        let solutionFile = sprintf "%s%d.isl" solutionDir best
        parseSolutionFile solutionFile

let printSolutionStats () =
    let scores = [ for i in [ 1..40] ->
                    let taskPath = sprintf "tasks/%d.png" i
                    let targetImage, initCanvas = prepareTask taskPath
                    let solution = loadBestSolution taskPath
                    (i, scoreSolution targetImage initCanvas solution) ]
    for (task, (cost, similarity)) in scores do
        printfn "Task %d penalty: %d\tCost: %d\tSimilarity: %d" task (cost+similarity) cost similarity 
    let totalCost = scores |> List.map (fun (_, (cost, _)) -> cost) |> List.sum
    let totalSimilarity = scores |> List.map (fun (_, (_, similarity)) -> similarity) |> List.sum
    let totalPenalty = totalCost + totalSimilarity
    printfn "Total: %d\tCost: %d\tSimilarity: %d" totalPenalty totalCost totalSimilarity
        
let optimizeAll taskPath targetImage initCanvas =
    let solutionDir = getSolutionDir taskPath
    let dirinfo = IO.Directory.CreateDirectory solutionDir
    let allSolutions =
        dirinfo.EnumerateFiles ()
        |> Seq.filter (fun f -> f.Extension = ".isl")
        |> Seq.map (fun f -> f.Name.Split('.')[0] |> int)
        |> Seq.map (fun s -> parseSolutionFile $"{solutionDir}{s}.isl")
    printfn "Optimizing %d solutions" (Seq.length allSolutions)
    allSolutions
    |> Seq.iter (fun solution ->
        let optimized = OptiTrace.optimize targetImage initCanvas solution
        let cost, similarity = scoreSolution targetImage initCanvas optimized
        writeSolutionIfBetter taskPath optimized (cost+similarity))
    ()


type Arguments =
    | GUI
    | [<AltCommandLine("-v")>] Verbose
    | AI of AISelector option
    | NoOptiTrace
    | OptimizeAll
    | SplitPoint of SplitPointSelector option
    | Repetitions of int
    | MergeAI of AISelector option
    | RandomSeed of int
    | Stats
    | [<MainCommand; ExactlyOnce; Last>] TaskPath of task:string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | GUI -> "Show the GUI"
            | AI _ -> "The AI to use. If no AI set, load the best solution on disk."
            | NoOptiTrace _ -> "Disable the opti-tracer on the generated solution."
            | OptimizeAll _ -> "Run optimizer on all existing solutions for that task"
            | SplitPoint _ -> "The split point to use"
            | Repetitions _ -> "The number of repetitions"
            | TaskPath _ -> "The task png path to use"
            | Verbose _ -> "Print more information"
            | MergeAI _ -> "The AI to use inside the mergeMeta strategy, if chosen"
            | RandomSeed _ -> "The random seed to use"
            | Stats -> "Don't do anything, just print some stats on the best solution"

and AISelector = OneLiner | QuadTree | Random | MCTS | EagerSwapper | AssignSwapper | MergeAll | MergeMeta
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
    assert (Map.count canvas.topBlocks = 1) // Random does not support non-blank initial canvas
    let initialBlock = Map.keys canvas.topBlocks |> Seq.head
    let solution, solverCost, solverSimilarity = AI.fastRandomSolver initialBlock  {r=255;g=255;b=255;a=255} (sliceWholeImage targetImage) canvas
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
    let mutable bestSolution,_ = solver img canvas
    let mutable bestCost, bestSim = scoreSolution img canvas bestSolution
    for _ in 1..n do
        let solution,_ = solver img canvas
        let cost, similarity = scoreSolution img canvas solution
        if cost + similarity < bestCost + bestSim then
            bestSolution <- solution
            bestCost <- cost
            bestSim <- similarity
    bestSolution, Some (bestCost, bestSim)

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
    if results.Contains Stats then
        printSolutionStats ()
        0
    else
    let randomSeed = 
        match results.TryGetResult RandomSeed with
        | Some s -> s
        | None -> System.Random().Next()
    printfn "Random seed: %d" randomSeed
    Rng.rng <- System.Random(randomSeed) // TODO: add a command-line option for overriding this
    let taskPath = results.GetResult (TaskPath)
    let targetImage, initCanvas = prepareTask taskPath
    if results.Contains OptimizeAll then
        optimizeAll taskPath targetImage initCanvas
        0
    else
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
        | AssignSwapper -> (Swapper.assignSwapperStrategy Swapper.simpleGridSwapping, "assign-swapper-simple")
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
        | MergeAll ->
            match results.GetResult (MergeAI) with
            | None -> failwith "MergeAll requires a MergeAI argument"
            | Some MergeMeta -> failwith "Can't use a merge strategy after merging everything"
            | Some mergeAI ->
                let innerSolver, innerSolverName = getSolver mergeAI
                (Merger.mergeAllMetaSolver innerSolver (not(results.Contains NoOptiTrace)),
                 $"merge-all({innerSolverName})")
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
    if results.Contains Verbose then
        printfn "Solution from the solver, before optimization:\n%s" (deparse solution)
    // Optionally optimize the solution
    if results.Contains NoOptiTrace then
        printfn "OptiTrace DISABLED!"
    else
        printfn "Optimizing..."
        solution <- OptiTrace.optimize targetImage initCanvas solution
    // Run the simulator to get the final score
    let (solutionScore, imageSimilarity) = scoreSolution targetImage initCanvas solution
    // Write it to disk if it was better
    writeSolutionIfBetter taskPath solution (solutionScore + imageSimilarity)
    // For the user: verbose output and/or GUI
    if results.Contains Verbose then
        printfn "Final solution instructions:\n%s" (deparse solution)
    if results.Contains GUI then
        showGui initCanvas targetImage solution |> ignore
    0
