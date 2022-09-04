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
            [ { est with r=byte(est.r-1uy) }
              { est with r=byte(est.r+1uy) }
              { est with g=byte(est.g-1uy) }
              { est with g=byte(est.g+1uy) }
              { est with b=byte(est.b-1uy) }
              { est with b=byte(est.b+1uy) }
              { est with a=byte(est.a-1uy) }
              { est with a=byte(est.a+1uy) } ]
            |> List.map (fun n -> 
                let dist = colorDistanceSeq cols n
                printfn "Neighbour %s distance: %f" (n.toString ()) dist
                dist
                )
            |> List.min
        Assert.IsTrue(estDist <= neigDist)

    [<TestMethod>]
    member this.medianColorLst1() =
        let cols = [ { r=1uy; g=2uy; b=3uy; a=4uy } ]
        let median = medianColorSeq cols
        Assert.AreEqual(cols[0], median)

    [<TestMethod>]
    member this.medianColorLst2() =
        let cols = [ { r=1uy; g=1uy; b=1uy; a=1uy }
                     { r=3uy; g=3uy; b=3uy; a=3uy } ]
        let median = medianColorSeq cols
        Assert.AreEqual({ r=2uy; g=2uy; b=2uy; a=2uy }, median)

    [<TestMethod>]
    member this.medianColorLst3() =
        let cols = [ { r=0uy; g=0uy; b=0uy; a=0uy }
                     { r=0uy; g=0uy; b=0uy; a=0uy }
                     { r=0uy; g=0uy; b=0uy; a=0uy }
                     { r=0uy; g=0uy; b=0uy; a=12uy } ]
        let median = medianColorSeq cols
        Assert.AreEqual( { r=0uy; g=0uy; b=0uy; a=0uy }, median)
        testEstNeighbors cols median

    [<TestMethod>]
    member this.medianColorLst4() =
        let cols = [ { r=0uy; g=1uy; b=2uy; a=3uy }
                     { r=8uy; g=9uy; b=10uy; a=11uy }
                     { r=16uy; g=17uy; b=18uy; a=19uy }
                     { r=24uy; g=25uy; b=26uy; a=27uy }
                     { r=32uy; g=33uy; b=34uy; a=35uy } ]
        let median = medianColorSeq cols
        testEstNeighbors cols median

    [<TestMethod>]
    member this.medianColorLstEquals() =
        let cols = Array.init (400*400) (fun _ -> { r=255uy; g=255uy; b=255uy; a=255uy })
        let median = medianColorSeq cols
        Assert.AreEqual( { r=255uy; g=255uy; b=255uy; a=255uy }, median)
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
            fcols |> List.map (fun (r, g, b, a) -> { r=byte r; g=byte g; b=byte b; a=byte a })
        let median = medianColorSeq cols
        testEstNeighbors cols median

