module Util

open Model 

// This should really compute the geometric median, but that's more complicated
// https://en.wikipedia.org/wiki/Geometric_median
let median_color (img: ImageSlice) : Color =
    let mutable sum_r: int = 0
    let mutable sum_g: int = 0
    let mutable sum_b: int = 0
    let mutable sum_a: int = 0
    for x in 0 .. img.size.width do
        for y in 0 .. img.size.height do
            let c = color_at_pos img {x=x; y=y}
            sum_r <- sum_r + int c.r
            sum_g <- sum_g + int c.g
            sum_b <- sum_b + int c.b
            sum_a <- sum_a + int c.a
    let area = area img.size
    {r=byte(sum_r/area); g=byte (sum_g/area); b=byte (sum_b/area); a=byte(sum_a/area)}