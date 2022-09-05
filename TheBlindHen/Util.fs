module Util

open Model 

let imageSlicePixels (img: ImageSlice) : Color[] =
    let cols = Array.zeroCreate (img.size.width * img.size.height)
    for x in 0 .. img.size.width-1 do
        for y in 0 .. img.size.height-1 do
            let c = colorAtPos img {x=x; y=y}
            cols.[y * img.size.width + x] <- c
    cols

let averageColor_f (cols: Color seq) : (float*float*float*float) =
    let (sum_r, sum_g, sum_b, sum_a, len) =
        cols
        |> Seq.fold (fun (sum_r, sum_g, sum_b, sum_a, len) col ->
            (sum_r + float col.r, sum_g + float col.g, sum_b + float col.b, sum_a + float col.a, len+1)
        ) (0.0, 0.0, 0.0, 0.0, 0)
    let len = float len
    (float sum_r/len, float sum_g/len, float sum_b/len, float sum_a/len)

let averageColorOfImg (img: ImageSlice) : (float*float*float*float) =
    let mutable sum_r, sum_h, sum_b, sum_a, len = 0L, 0L, 0L, 0L, 0L
    for y in 0 .. img.size.height-1 do
        for x in 0 .. img.size.width-1 do
            let c = colorAtPos img {x=x; y=y}
            sum_r <- sum_r + int64 c.r
            sum_h <- sum_h + int64 c.g
            sum_b <- sum_b + int64 c.b
            sum_a <- sum_a + int64 c.a
            len <- len + 1L
    (
        System.Math.Round(float sum_r / float len),
        System.Math.Round(float sum_h / float len),
        System.Math.Round(float sum_b / float len),
        System.Math.Round(float sum_a / float len)
    )

let averageColor (img: ImageSlice) : Color =
    let (r_f, g_f, b_f, a_f) = averageColorOfImg img
    {r=int(r_f); g=int(g_f); b=int(b_f); a=int(a_f)}

let approxAverageColor (img: ImageSlice) : Color =
    let area = img.size.height * img.size.width
    if area <= 100 then averageColor img else
    let cutoffpoint = if area <= 500 then 50 else 100
    let mutable r, g, b, a = 0.0, 0.0, 0.0, 0.0
    for _ in 1 .. cutoffpoint do
        let x = Rng.rng.Next(img.size.width)
        let y = Rng.rng.Next(img.size.height)
        let c = colorAtPos img {x=x; y=y}
        r <- r + float c.r
        g <- g + float c.g
        b <- b + float c.b
        a <- a + float c.a
    {r=int(r / float cutoffpoint); g=int(g / float cutoffpoint); b=int(b / float cutoffpoint); a=int(a / float cutoffpoint)}

// Treat c1 and c2 as 4-dimensional vectors and compute the Euclidean distance
let colorDistance (c1: Color) (c2: Color) : float =
    let r = int c1.r - int c2.r
    let g = int c1.g - int c2.g
    let b = int c1.b - int c2.b
    let a = int c1.a - int c2.a
    sqrt(float (r*r + g*g + b*b + a*a))

let colorDistanceSeq (cols: Color seq) (est: Color) =
    cols
    |> Seq.map (colorDistance est)
    |> Seq.sum

let medianColorSeq (cols: Color seq) : Color =
    let fcols = cols
                |> List.ofSeq
                |> List.map (fun c -> (float c.r, float c.g, float c.b, float c.a))
    if fcols.Length = 0 then
        printfn "WARNING: medianColorSeq called with empty list"
        {r=0; g=0; b=0; a=0}
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
        // Experimentally, a value of 0.1 gives the same results as a value of
        // 0.01 on the first 9 test images. A value of 1.0 gives worse results
        // but not terrible.
        let estDist = fdist est est'
        if i > 100 then
            printfn "WARNING: medianColor did not converge (dist=%f)" estDist
            est'
        else if estDist < 0.2 then
            est'
        else
            loop (i+1) est'
    // Choose an initial point that is unlikely to let us ever end up on one of
    // the integral input points
    let (avg0, avg1, avg2, avg3) = averageColor_f cols
    let init_est = ( avg0 - 0.234203952,
                     avg1 + 0.23942323,
                     avg2 - 0.1102114,
                     avg3 + 0.383523269 )
    let fmid0, fmid1, fmid2, fmid3 = loop 0 init_est
    if fmid0 < -0.1 || fmid0 > 255.1 ||
       fmid1 < -0.1 || fmid1 > 255.1 ||
       fmid2 < -0.1 || fmid2 > 255.1 || 
       fmid3 < -0.1 || fmid3 > 255.1 then
        printfn "WARNING: median_color computed an invalid color: (%f, %f, %f, %f)" fmid0 fmid1 fmid2 fmid3
    let est = {r=int(System.Math.Round(fmid0));
               g=int(System.Math.Round(fmid1));
               b=int(System.Math.Round(fmid2));
               a=int(System.Math.Round(fmid3))}
    let estDist = colorDistanceSeq cols est
    // Try taking a single neighbouring step in each direction
    let (bestNeigh, bestNeighDist) = 
        // These operations may overflow, but it shouldn't matter: we're
        // testing whether we've found the minimum by trying neighbours, so
        // no harm in trying a few other colors as well on overflow.
        [ { est with r=int(est.r-1) }
          { est with r=int(est.r+1) }
          { est with g=int(est.g-1) }
          { est with g=int(est.g+1) }
          { est with b=int(est.b-1) }
          { est with b=int(est.b+1) }
          { est with a=int(est.a-1) }
          { est with a=int(est.a+1) } ]
        |> List.map (fun n -> 
            let dist = colorDistanceSeq cols n
            (n, dist))
        |> List.minBy snd
    if bestNeighDist < estDist then
        bestNeigh
    else
        est

/// Compute the median color of the image slice, using Weiszfeld's algorithm
let medianColor (img: ImageSlice) : Color =
    medianColorSeq (imageSlicePixels img)

/// Returns the most frequent color and the number of pixels with that color
let mostFrequentColor (img: ImageSlice) : Color * int =
    let counts = new System.Collections.Generic.Dictionary<Color, int>(img.size.height * img.size.width)
    for y in 0 .. img.size.height-1 do
        for x in 0 .. img.size.width-1 do
            let c = colorAtPos img {x=x; y=y}
            if counts.ContainsKey c then
                counts.[c] <- counts.[c] + 1
            else
                counts.[c] <- 1
    let max = counts |> Seq.maxBy (fun kv -> kv.Value)
    max.Key, max.Value

let approxMostFrequentColor (img: ImageSlice): Color =
    let area = img.size.height * img.size.width
    if area <= 100 then mostFrequentColor img |> fst else
    let cutoffpoint = if area <= 500 then 50 else 100
    let counts = new System.Collections.Generic.Dictionary<Color, int>(img.size.height * img.size.width)
    for _ in 1 .. cutoffpoint do
        let x = Rng.rng.Next(img.size.width)
        let y = Rng.rng.Next(img.size.height)
        let c = colorAtPos img {x=x; y=y}
        if counts.ContainsKey c then
            counts.[c] <- counts.[c] + 1
        else
            counts.[c] <- 1
    let max = counts |> Seq.maxBy (fun kv -> kv.Value)
    max.Key

/// If the number of pixels with the most frequent color is above a certain
/// threshold, return that color.  Otherwise, return the median color.
let approxMedianColor (img: ImageSlice) : Color =
    let c, n = mostFrequentColor img
    if n > 200 then
        c
    else
        medianColor img

/// If the number of pixels with the most frequent color is above a certain
/// threshold, return that color.  Otherwise, return the average color.
let frequentOrAverageColor (img: ImageSlice) : Color =
    let c, n = mostFrequentColor img
    if n > 200 then
        c
    else
        averageColor img

let distanceScalingFactor = 0.005

let distanceToSimilarity distance =
    int (System.Math.Round (distance * distanceScalingFactor))

/// The distance between the target block and a constant color
/// Returns a dinstance, meaning it's _not_ been scaled by distanceScalingFactor and rounded
let singleColorDistance (proposal: Color) (target: ImageSlice) : float =
    let mutable score = 0.0
    for y in 0 .. target.size.height-1 do
        for x in 0 .. target.size.width-1 do
            let c2 = colorAtPos target {x=x; y=y}
            score <- score + colorDistance proposal c2
    score

/// The approximate distance between the target block and a constant color
/// Returns a dinstance, meaning it's _not_ been scaled by distanceScalingFactor and rounded
let approxSingleColorDistance (proposal: Color) (target: ImageSlice) : float =
    let area = target.size.height * target.size.width
    if area <= 100 then singleColorDistance proposal target else
    let cutoffpoint = if area <= 500 then 50 else 100
    let mutable score = 0.0
    for _ in 1 .. cutoffpoint do
        let x = Rng.rng.Next(target.size.width)
        let y = Rng.rng.Next(target.size.height)
        let c = colorAtPos target {x=x; y=y}
        score <- score + colorDistance proposal c
    (score / float cutoffpoint) * float area

let singleColorSimilarity (proposal: Color) (target: ImageSlice) : int =
    int (System.Math.Round (singleColorDistance proposal target * distanceScalingFactor))

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

/// Returns a similarity, meaning it has been scaled by distanceScalingFactor and rounded
let imageSimilarity (proposal: ImageSlice) (target: ImageSlice) : int =
    distanceToSimilarity (subImageDistance proposal target)

/// If the canvas is sub-divided as a grid, return the size of the grid
/// elements, and the number of horizontal and vertical grid elements
let canvasGridInfo canvas : GridInfo option =
    let b0 = canvas.topBlocks["0"]
    match Map.values canvas.topBlocks |> Seq.tryFind (fun b -> b.size <> b0.size) with
    | Some _ -> None
    | None ->
        Some ({
            cellSize = b0.size;
            dimensions = { width = canvas.size.width / b0.size.width;
                           height = canvas.size.height / b0.size.height } })

/// Create a permanent numbering of each location of a block in the canvas.
/// This is mainly used for initialized grid-like canvases
let positionMap (canvas: Canvas) =
    let (_, blockMap, positions) =
        Map.values canvas.topBlocks
        |> List.ofSeq
        |> List.sortBy (fun b -> b.lowerLeft.y * canvas.size.width + b.lowerLeft.x)
        |> Seq.fold (fun (n, blockMap, positionMap) block ->
                (n+1,
                Map.add n (block :?> SimpleBlock) blockMap,
                Map.add n (block.lowerLeft, block.size) positionMap)
            ) (0, Map.empty, Map.empty)
    (blockMap, positions)