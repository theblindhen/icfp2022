namespace UnitTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Model

[<TestClass>]
type ModelTests () =

    [<TestMethod>]
    member this.Render1x2SimpleBlock () =
        let b = SimpleBlock("0", {width=1; height=2}, {x=3; y=4}, {r=5; g=6; b=7; a=8})
        let img = renderBlock b
        Assert.AreEqual(1, img.size.width)
        Assert.AreEqual(2, img.size.height)
        Assert.AreEqual(5, img.pixels.[0].r)
        Assert.AreEqual(5, img.pixels.[1].r)

    [<TestMethod>]
    member this.Render2x2ComplexBlock () =
        let b = ComplexBlock("0", {width=2; height=2}, {x=3; y=4}, [|
            SimpleBlock("1", {width=1; height=1}, {x=3; y=4}, {r=5; g=6; b=7; a=8})
            SimpleBlock("2", {width=1; height=1}, {x=4; y=4}, {r=9; g=10; b=11; a=12})
            SimpleBlock("3", {width=1; height=1}, {x=3; y=5}, {r=13; g=14; b=15; a=16})
            SimpleBlock("4", {width=1; height=1}, {x=4; y=5}, {r=17; g=18; b=19; a=20})
        |])
        let img = renderBlock b
        Assert.AreEqual(2, img.size.width)
        Assert.AreEqual(2, img.size.height)
        Assert.AreEqual(5, img.pixels.[0].r)
        Assert.AreEqual(10, img.pixels.[1].g)
        Assert.AreEqual(15, img.pixels.[2].b)
        Assert.AreEqual(20, img.pixels.[3].a)

