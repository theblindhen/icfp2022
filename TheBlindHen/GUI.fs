module GUI

let WINDOWSIZE = 800.0
let CANVASWIDTH = 400.0
let CANVASHEIGHT = 400.0

open System
open Model

type Globals = {
    mutable imagePath: string
    mutable canvas: Canvas option
}

let globals = {
    imagePath = ""
    canvas = None
}

module MVU =
    open Elmish
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.FuncUI.DSL
    open Avalonia.Controls.Shapes
    open Avalonia.Media.Imaging

    type State = {
        TargetBitmap: Bitmap
        Canvas: Model.Canvas
    }

    type Msg = Id

    let init (imagePath: string, canvas: Model.Canvas): State * Cmd<Msg> =
        {
            TargetBitmap = new Avalonia.Media.Imaging.Bitmap(imagePath)
            Canvas = canvas
        },
        Cmd.none

    let update (msg: Msg) (state: State): (State * Cmd<Msg>) =
        match msg with
        | Id -> state, Cmd.none

    let viewCanvas (state: State) =
        state.Canvas.topBlocks
        |> Map.toList
        |> List.map (fun (id, block) ->
            let blockSize, lowerLeft = block.size, block.lowerLeft
            [
                Line.create [
                   Line.startPoint (float lowerLeft.x, CANVASHEIGHT - float lowerLeft.y)
                   Line.endPoint (float (lowerLeft.x + blockSize.width), CANVASHEIGHT - float (lowerLeft.y))
                   Line.strokeThickness 2.0
                   Line.stroke "#000000"
                ] :> Avalonia.FuncUI.Types.IView
                Line.create [
                   Line.startPoint (float lowerLeft.x, CANVASHEIGHT - float lowerLeft.y)
                   Line.endPoint (float lowerLeft.x, CANVASHEIGHT - float (lowerLeft.y + blockSize.height))
                   Line.strokeThickness 2.0
                   Line.stroke "#000000"
                ] :> Avalonia.FuncUI.Types.IView

            ])
        |> List.concat

    let viewTarget (state: State) =
        [
            Image.create [
                Image.source state.TargetBitmap
            ] :> Avalonia.FuncUI.Types.IView
        ]

    let view (state: State) (dispatch) =
        DockPanel.create [
            DockPanel.children [ 
                Canvas.create [
                    Canvas.background "#2c3e50"
                    Canvas.children (viewTarget state @ viewCanvas state)
                ]       
            ]
        ]       

open Elmish
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts

type MainWindow() as this =
    inherit HostWindow()
    do
        base.Title <- "TestUI"
        base.Width <- WINDOWSIZE
        base.Height <- WINDOWSIZE

        Program.mkProgram MVU.init MVU.update MVU.view
        |> Program.withHost this
        |> Program.runWith (globals.imagePath, Option.get globals.canvas)

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Load "avares://Avalonia.Themes.Default/DefaultTheme.xaml"
        this.Styles.Load "avares://Avalonia.Themes.Default/Accents/BaseDark.xaml"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

let showGui imgPath canvas =
    globals.imagePath <- imgPath
    globals.canvas <- Some canvas
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .UseSkia()
        .StartWithClassicDesktopLifetime([||]) // TODO: how to parse args?