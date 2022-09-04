module Swapper

open Model

let assertAllBlockSameSize canvas =
    let b0 = Map.find "0" canvas.topBlocks
    for b in Map.values canvas.topBlocks do
        assert (b.size = b0.size)

let eagerSwapper (targetImage: Image) (canvas: Canvas) =
    assertAllBlockSameSize canvas
    ([], -1, -1)