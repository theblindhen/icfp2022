module Rng

/// The global RNG that everything should use so we get reproducibility
let mutable rng = System.Random(0) // overwritten in Program.fs
