namespace UnitTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Model

[<TestClass>]
type ModelTests () =

    [<TestMethod>]
    member this.Render1x2SimpleBlock () =
        let b = SimpleBlock("0", {width=1; height=2}, {x=3; y=4}, {r=5uy; g=6uy; b=7uy; a=8uy})
        let img = renderBlock b
        Assert.AreEqual(1, img.size.width)
        Assert.AreEqual(2, img.size.height)
        Assert.AreEqual(5uy, img.pixels.[0].r)
        Assert.AreEqual(5uy, img.pixels.[1].r)

    [<TestMethod>]
    member this.Render2x2ComplexBlock () =
        let b = ComplexBlock("0", {width=2; height=2}, {x=3; y=4}, [|
            SimpleBlock("1", {width=1; height=1}, {x=3; y=4}, {r=5uy; g=6uy; b=7uy; a=8uy})
            SimpleBlock("2", {width=1; height=1}, {x=4; y=4}, {r=9uy; g=10uy; b=11uy; a=12uy})
            SimpleBlock("3", {width=1; height=1}, {x=3; y=5}, {r=13uy; g=14uy; b=15uy; a=16uy})
            SimpleBlock("4", {width=1; height=1}, {x=4; y=5}, {r=17uy; g=18uy; b=19uy; a=20uy})
        |])
        let img = renderBlock b
        Assert.AreEqual(2, img.size.width)
        Assert.AreEqual(2, img.size.height)
        Assert.AreEqual(5uy, img.pixels.[0].r)
        Assert.AreEqual(10uy, img.pixels.[1].g)
        Assert.AreEqual(15uy, img.pixels.[2].b)
        Assert.AreEqual(20uy, img.pixels.[3].a)

