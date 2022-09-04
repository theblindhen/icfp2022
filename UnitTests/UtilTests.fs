namespace UnitTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Model
open Util

[<TestClass>]
type UtilTests () =

    let testEstNeighbors (cols: Color seq) (est: Color) = 
        let estDist = colorDistanceSeq cols est
        printfn "Estimated color: %s. Distance %f" (est.toString ()) estDist
        let neigDist = 
            // These operations may overflow, but it shouldn't matter: we're
            // testing whether we've found the minimum by trying neighbours, so
            // no harm in trying a few other colors as well on overflow.
            [ { est with r=est.r-1 }
              { est with r=est.r+1 }
              { est with g=est.g-1 }
              { est with g=est.g+1 }
              { est with b=est.b-1 }
              { est with b=est.b+1 }
              { est with a=est.a-1 }
              { est with a=est.a+1 } ]
            |> List.map (fun n -> 
                let dist = colorDistanceSeq cols n
                printfn "Neighbour %s distance: %f" (n.toString ()) dist
                dist
                )
            |> List.min
        Assert.IsTrue(estDist <= neigDist)

    [<TestMethod>]
    member this.medianColorLst1() =
        let cols = [ { r=1; g=2; b=3; a=4 } ]
        let median = medianColorSeq cols
        Assert.AreEqual(cols[0], median)

    [<TestMethod>]
    member this.medianColorLst2() =
        let cols = [ { r=1; g=1; b=1; a=1 }
                     { r=3; g=3; b=3; a=3 } ]
        let median = medianColorSeq cols
        Assert.AreEqual({ r=2; g=2; b=2; a=2 }, median)

    [<TestMethod>]
    member this.medianColorLst3() =
        let cols = [ { r=0; g=0; b=0; a=0 }
                     { r=0; g=0; b=0; a=0 }
                     { r=0; g=0; b=0; a=0 }
                     { r=0; g=0; b=0; a=12 } ]
        let median = medianColorSeq cols
        Assert.AreEqual( { r=0; g=0; b=0; a=0 }, median)
        testEstNeighbors cols median

    [<TestMethod>]
    member this.medianColorLst4() =
        let cols = [ { r=0; g=1; b=2; a=3 }
                     { r=8; g=9; b=10; a=11 }
                     { r=16; g=17; b=18; a=19 }
                     { r=24; g=25; b=26; a=27 }
                     { r=32; g=33; b=34; a=35 } ]
        let median = medianColorSeq cols
        testEstNeighbors cols median

    [<TestMethod>]
    member this.medianColorLstEquals() =
        let cols = Array.init (400*400) (fun _ -> { r=255; g=255; b=255; a=255 })
        let median = medianColorSeq cols
        Assert.AreEqual( { r=255; g=255; b=255; a=255 }, median)
        testEstNeighbors cols median

    [<TestMethod>]
    member this.medianColorLstTask10() =
        let imgPath = "../../../../tasks/10.png"
        let img = Loader.loadPNG imgPath
        let median = medianColor (sliceWholeImage img)
        testEstNeighbors img.pixels median

    [<TestMethod>]
    member this.medianColorLstTask14() =
        let imgPath = "../../../../tasks/14.png"
        let img = Loader.loadPNG imgPath
        let median = medianColor (sliceWholeImage img)
        testEstNeighbors img.pixels median

    [<TestMethod>]
    member this.medianColorLst5() =
        let fcols = [
            (237.0, 241.0, 247.0, 255.0); (255.0, 255.0, 255.0, 255.0);
            (150.0, 154.0, 159.0, 255.0); (0.0, 0.0, 0.0, 255.0); (2.0, 6.0, 11.0, 255.0);
            (0.0, 4.0, 9.0, 255.0); (255.0, 253.0, 252.0, 255.0);
            (255.0, 255.0, 255.0, 255.0); (161.0, 160.0, 158.0, 255.0);
            (0.0, 0.0, 0.0, 255.0); (2.0, 1.0, 0.0, 255.0); (0.0, 0.0, 0.0, 255.0);
            (250.0, 250.0, 250.0, 255.0); (255.0, 255.0, 255.0, 255.0);
            (158.0, 158.0, 158.0, 255.0); (0.0, 0.0, 0.0, 255.0); (2.0, 2.0, 3.0, 255.0);
            (0.0, 0.0, 0.0, 255.0); (251.0, 251.0, 251.0, 255.0);
            (255.0, 255.0, 255.0, 255.0); (158.0, 158.0, 158.0, 255.0);
            (0.0, 0.0, 0.0, 255.0); (2.0, 2.0, 2.0, 255.0); (0.0, 0.0, 0.0, 255.0);
            (251.0, 251.0, 251.0, 255.0); (255.0, 255.0, 255.0, 255.0);
            (158.0, 158.0, 158.0, 255.0); (0.0, 0.0, 0.0, 255.0); (2.0, 2.0, 2.0, 255.0);
            (0.0, 0.0, 0.0, 255.0); (251.0, 251.0, 251.0, 255.0);
            (255.0, 255.0, 255.0, 255.0); (158.0, 158.0, 158.0, 255.0);
            (0.0, 0.0, 0.0, 255.0); (2.0, 2.0, 2.0, 255.0); (0.0, 0.0, 0.0, 255.0);
            (251.0, 251.0, 251.0, 255.0); (255.0, 255.0, 255.0, 255.0);
            (158.0, 158.0, 158.0, 255.0); (0.0, 0.0, 0.0, 255.0); (2.0, 2.0, 2.0, 255.0);
            (0.0, 0.0, 0.0, 255.0)]
        let cols =
            fcols |> List.map (fun (r, g, b, a) -> { r=int r; g=int g; b=int b; a=int a })
        let median = medianColorSeq cols
        testEstNeighbors cols median

