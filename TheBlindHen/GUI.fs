module GUI

let WINDOWSIZE = 800.0

open System

let imgPathGlobal : string ref = ref ""

module MVU =
    open Elmish
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.FuncUI.DSL
    open Avalonia.Controls.Shapes
    open Avalonia.Media.Imaging

    type State = {
        TargetBitmap: Bitmap
    }

    type Msg = Id

    let init (imagePath: string): State * Cmd<Msg> =
        {
            TargetBitmap = new Avalonia.Media.Imaging.Bitmap(imagePath)
        },
        Cmd.none

    let update (msg: Msg) (state: State): (State * Cmd<Msg>) =
        match msg with
        | Id -> state, Cmd.none
            
    let view (state: State) (dispatch) =
        DockPanel.create [
            DockPanel.children [ 
                Canvas.create [
                    Canvas.background "#2c3e50"
                    Canvas.children [
                        Image.create [
                            Image.source state.TargetBitmap
                        ]
                    ]
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
        |> Program.runWith (!imgPathGlobal)

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

let showGui imgPath =
    imgPathGlobal := imgPath
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .UseSkia()
        .StartWithClassicDesktopLifetime([||]) // TODO: how to parse args?