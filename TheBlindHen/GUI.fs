module GUI

let WINDOWSIZE = 800.0
let CANVASWIDTH = 400
let CANVASHEIGHT = 400

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
        Scale: int
    }

    type Msg = ZoomIn | ZoomOut

    let init (imagePath: string, canvas: Model.Canvas): State * Cmd<Msg> =
        {
            TargetBitmap = new Avalonia.Media.Imaging.Bitmap(imagePath)
            Canvas = canvas
            Scale = 1
        },
        Cmd.none

    let update (msg: Msg) (state: State): (State * Cmd<Msg>) =
        match msg with
        | ZoomIn -> { state with Scale = state.Scale + 1 }, Cmd.none
        | ZoomOut -> { state with Scale = max (state.Scale - 1) 1 }, Cmd.none

    let viewCanvas (state: State) =
        state.Canvas.topBlocks
        |> Map.toList
        |> List.map (fun (id, block) ->
            let blockSize, lowerLeft = block.size, block.lowerLeft
            [
                Line.create [
                   Line.startPoint (float lowerLeft.x, float (CANVASHEIGHT - lowerLeft.y))
                   Line.endPoint (float (lowerLeft.x + blockSize.width), float (CANVASHEIGHT - lowerLeft.y))
                   Line.strokeThickness 2.0
                   Line.stroke "#000000"
                ] :> Avalonia.FuncUI.Types.IView
                Line.create [
                   Line.startPoint (float lowerLeft.x, float (CANVASHEIGHT - lowerLeft.y))
                   Line.endPoint (float lowerLeft.x, float (CANVASHEIGHT - (lowerLeft.y + blockSize.height)))
                   Line.strokeThickness 2.0
                   Line.stroke "#000000"
                ] :> Avalonia.FuncUI.Types.IView
            ])
        |> List.concat

    let viewTarget (state: State) =
        let pixelSize = new Avalonia.PixelSize(CANVASHEIGHT * state.Scale, CANVASWIDTH * state.Scale)
        let scaledBitmap = state.TargetBitmap.CreateScaledBitmap(pixelSize, Avalonia.Visuals.Media.Imaging.BitmapInterpolationMode.LowQuality) 
        let scaledCroppedBitmap = new CroppedBitmap(scaledBitmap, new Avalonia.PixelRect(0, 0, 400, 400))
        [
            Image.create [
                Image.source scaledCroppedBitmap
            ] :> Avalonia.FuncUI.Types.IView
        ]

    let view (state: State) (dispatch) =
        DockPanel.create [
            DockPanel.children [ 
                UniformGrid.create [
                    UniformGrid.dock Dock.Bottom
                    UniformGrid.columns 2
                    UniformGrid.children [
                        Button.create [
                            Button.onClick (fun _ -> dispatch ZoomOut)
                            Button.content "Zoom out"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch ZoomIn)
                            Button.content "Zoom in"
                        ]
                    ]
                ]
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
        .StartWithClassicDesktopLifetime([||])