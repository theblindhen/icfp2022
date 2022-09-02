open Loader
open GUI

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Specify path to target image as first argument"
        1
    else
        let imgPath = args[0]
        showGui imgPath |> ignore
        0
