module Util

open Model 

// This should really compute the geometric median, but that's more complicated
// https://en.wikipedia.org/wiki/Geometric_median
let median_color (img: ImageSlice) : Color =
    let mutable sum_r: int = 0
    let mutable sum_g: int = 0
    let mutable sum_b: int = 0
    let mutable sum_a: int = 0
    for x in 0 .. img.size.width-1 do
        for y in 0 .. img.size.height-1 do
            let c = color_at_pos img {x=x; y=y}
            sum_r <- sum_r + int c.r
            sum_g <- sum_g + int c.g
            sum_b <- sum_b + int c.b
            sum_a <- sum_a + int c.a
    let area = area img.size
    {r=byte(sum_r/area); g=byte (sum_g/area); b=byte (sum_b/area); a=byte(sum_a/area)}

// Treat c1 and c2 as 4-dimensional vectors and compute the Euclidean distance
let color_distance (c1: Color) (c2: Color) : float =
    let r = int c1.r - int c2.r
    let g = int c1.g - int c2.g
    let b = int c1.b - int c2.b
    let a = int c1.a - int c2.a
    sqrt(float (r*r + g*g + b*b + a*a))

let image_distance (proposal: ImageSlice) (target: ImageSlice) : int =
    assert (proposal.size = target.size)
    let mutable score = 0.0
    for x in 0 .. proposal.size.width-1 do
        for y in 0 .. proposal.size.height-1 do
            let c1 = color_at_pos proposal {x=x; y=y}
            let c2 = color_at_pos target {x=x; y=y}
            score <- score + color_distance c1 c2
    int (score * 0.005) // TODO: Should probably be round?
