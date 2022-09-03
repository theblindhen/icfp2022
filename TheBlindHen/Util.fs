module Util

open Model 

// This should really compute the geometric median, but that's more complicated
// https://en.wikipedia.org/wiki/Geometric_median
let averageColor (img: ImageSlice) : Color =
    let mutable sum_r: int = 0
    let mutable sum_g: int = 0
    let mutable sum_b: int = 0
    let mutable sum_a: int = 0
    for x in 0 .. img.size.width-1 do
        for y in 0 .. img.size.height-1 do
            let c = colorAtPos img {x=x; y=y}
            sum_r <- sum_r + int c.r
            sum_g <- sum_g + int c.g
            sum_b <- sum_b + int c.b
            sum_a <- sum_a + int c.a
    let area = area img.size
    {r=byte(sum_r/area); g=byte (sum_g/area); b=byte (sum_b/area); a=byte(sum_a/area)}

let medianColorSeq (cols: Color seq) : Color =
    let fcols = cols
                |> List.ofSeq
                |> List.map (fun c -> (float c.r, float c.g, float c.b, float c.a))
    if fcols.Length = 0 then
        printfn "WARNING: medianColorSeq called with empty list"
        {r=0uy; g=0uy; b=0uy; a=0uy}
    elif fcols.Length = 1 then
        cols |> Seq.head
    else
    let fdist (a0,a1,a2,a3) (b0,b1,b2,b3) =
        let d0 = a0 - b0
        let d1 = a1 - b1
        let d2 = a2 - b2
        let d3 = a3 - b3
        sqrt(d0*d0 + d1*d1 + d2*d2 + d3*d3)
    let rec loop i est =
        if i > 100 then
            printfn "WARNING: median_color did not converge after 100 iterations"
            est
        else
        let w0,w1,w2,w3,N =
            fcols |> List.fold (fun (s0, s1, s2, s3, N) c ->
                let d = 1. / fdist c est
                let c0,c1,c2,c3 = c
                (s0 + c0 * d,
                 s1 + c1 * d,
                 s2 + c2 * d,
                 s3 + c3 * d,
                 N + d)
            ) (0., 0., 0., 0., 0.)
        let est' = (w0/N, w1/N, w2/N, w3/N)
        if fdist est est' < 0.01 then
            est'
        else
            loop (i+1) est'
    // Choose an initial point that is unlikely to let us ever end up on one of
    // the integral input points
    let init_est = ( 128.234203952,
                     127.23942323,
                     128.9102114,
                     127.983523269 )
    let fmid0, fmid1, fmid2, fmid3 = loop 0 init_est
    if fmid0 < -0.1 || fmid0 > 255.1 ||
       fmid1 < -0.1 || fmid1 > 255.1 ||
       fmid2 < -0.1 || fmid2 > 255.1 || 
       fmid3 < -0.1 || fmid3 > 255.1 then
        printfn "WARNING: median_color computed an invalid color: (%f, %f, %f, %f)" fmid0 fmid1 fmid2 fmid3
    {r=byte(System.Math.Round(fmid0));
     g=byte(System.Math.Round(fmid1));
     b=byte(System.Math.Round(fmid2));
     a=byte(System.Math.Round(fmid3))}

/// Compute the median color of the image slice, using Weiszfeld's algorithm
let medianColor (img: ImageSlice) : Color =
    // Convert the image slice to a list of colors
    let cols = Array.zeroCreate (img.size.width * img.size.height)
    for x in 0 .. img.size.width-1 do
        for y in 0 .. img.size.height-1 do
            let c = colorAtPos img {x=x; y=y}
            cols.[y * img.size.width + x] <- c
    medianColorSeq cols

// Treat c1 and c2 as 4-dimensional vectors and compute the Euclidean distance
let colorDistance (c1: Color) (c2: Color) : float =
    let r = int c1.r - int c2.r
    let g = int c1.g - int c2.g
    let b = int c1.b - int c2.b
    let a = int c1.a - int c2.a
    sqrt(float (r*r + g*g + b*b + a*a))

let distanceScalingFactor = 0.005

/// Returns a distance, meaning it's _not_ scaled by distanceScalingFactor
let subImageDistance (proposal: ImageSlice) (target: ImageSlice) : float =
    assert (proposal.size = target.size)
    let mutable score = 0.0
    for x in 0 .. proposal.size.width-1 do
        for y in 0 .. proposal.size.height-1 do
            let c1 = colorAtPos proposal {x=x; y=y}
            let c2 = colorAtPos target {x=x; y=y}
            score <- score + colorDistance c1 c2
    score

// Returns a similarity, meaning it has been scaled by distanceScalingFactor and rounded
let imageSimilarity (proposal: ImageSlice) (target: ImageSlice) : int =
    int (System.Math.Round (subImageDistance proposal target * distanceScalingFactor))
