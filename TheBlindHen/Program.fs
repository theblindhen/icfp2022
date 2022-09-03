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
        writeSolution taskPath solution (solution_cost + image_distance)
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
        writeSolution taskPath solution (solution_cost + image_distance)
        0
    else
        /// GUI
        let imgPath = args[0]
        let target = loadPNG imgPath
        showGui target canvas |> ignore
        0
