module GUI

let WINDOWSIZE = 800.0
let CANVASWIDTH = 400
let CANVASHEIGHT = 400

open System
open Model
open Instructions

type Globals = {
    mutable initCanvas: Model.Canvas option
    mutable target: Image option
    mutable solution: ISL list
}

let globals = {
    initCanvas = None
    target = None
    solution = []
}

module MVU =
    open Elmish
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.FuncUI.DSL
    open Avalonia.Controls.Shapes
    open Avalonia.Media.Imaging

    type State = {
        InitCanvas: Model.Canvas option
        Target: Model.Image
        Solution: ISL list
        Scale: int
        Step: int
    }

    type Msg = ZoomIn | ZoomOut | Step of int

    let init (initCanvas: Model.Canvas, target: Model.Image, solution: ISL list): State * Cmd<Msg> =
        {
            InitCanvas = Some(initCanvas)
            Target = target
            Solution = solution
            Scale = 1
            Step = 0
        },
        Cmd.none

    let update (msg: Msg) (state: State): (State * Cmd<Msg>) =
        match msg with
        | ZoomIn -> { state with Scale = state.Scale + 1 }, Cmd.none
        | ZoomOut -> { state with Scale = max (state.Scale - 1) 1 }, Cmd.none
        | Step i -> { state with Step = min (max (state.Step + i) 0) (List.length state.Solution) }, Cmd.none

    let viewSolution (state: State) =
        let (solution_canvas, solution_cost) = Instructions.simulate state.InitCanvas.Value (List.take state.Step state.Solution)
        let solution_image = renderCanvas solution_canvas
        let solution_bitmap = new Avalonia.Media.Imaging.Bitmap(Loader.toImageSharp solution_image |> Loader.toPngStream)
        [
            Image.create [
                Image.source solution_bitmap
            ] :> Avalonia.FuncUI.Types.IView
        ] @ (
            solution_canvas.topBlocks
            |> Map.toList
            |> List.map (fun (id, block) ->
                let blockSize, lowerLeft = block.size, block.lowerLeft
                let borderColor = "#333333"
                [
                    Line.create [
                    Line.startPoint (float lowerLeft.x, float (CANVASHEIGHT - lowerLeft.y))
                    Line.endPoint (float (lowerLeft.x + blockSize.width), float (CANVASHEIGHT - lowerLeft.y))
                    Line.strokeThickness 1.0
                    Line.stroke borderColor
                    ] :> Avalonia.FuncUI.Types.IView
                    Line.create [
                    Line.startPoint (float lowerLeft.x, float (CANVASHEIGHT - lowerLeft.y))
                    Line.endPoint (float lowerLeft.x, float (CANVASHEIGHT - (lowerLeft.y + blockSize.height)))
                    Line.strokeThickness 1.0
                    Line.stroke borderColor
                    ] :> Avalonia.FuncUI.Types.IView
                ])
            |> List.concat
        )

    let viewTarget (state: State) =
        let targetImg = Loader.toImageSharp state.Target
        Loader.resize { width = CANVASWIDTH * state.Scale; height = CANVASHEIGHT * state.Scale } targetImg
        let targetImg = new Avalonia.Media.Imaging.Bitmap(Loader.toPngStream targetImg)
        let scaledCroppedBitmap = new CroppedBitmap(targetImg, new Avalonia.PixelRect(0, 0, 400, 400))
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
                    UniformGrid.columns 4
                    UniformGrid.children [
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Step -10))
                            Button.content "<<"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Step -1))
                            Button.content "<"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Step 1))
                            Button.content ">"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Step 10))
                            Button.content ">>"
                        ]
                    ]
                ]
                Canvas.create [
                    Canvas.background "#2c3e50"
                    Canvas.left 0
                    Canvas.width 400
                    Canvas.children (viewTarget state)
                ]
                Canvas.create [
                    Canvas.background "#2c3e50"
                    Canvas.left 400
                    Canvas.children (viewSolution state)
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
        |> Program.runWith (Option.get globals.initCanvas, Option.get globals.target, globals.solution)

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

let showGui initCanvas target solution =
    globals.initCanvas <- Some(initCanvas)
    globals.target <- Some target
    globals.solution <- solution
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .UseSkia()
        .StartWithClassicDesktopLifetime([||])