import BIM

testAll = do
    testSolid
    testCrop
    testOverlap
    testScale
    testRotations
    testReflections

-- solid
testSolid = save "tests/outputs/solid.bmp" (solid navy (250,250))

-- crop
testCrop = do
    rubik <- load "img/rubik.bmp"
    let out = crop (50,200) (250, 20) rubik
    save "tests/outputs/crop.bmp" out

-- overlap
testOverlap = do
    rubik  <- load "img/rubik.bmp"
    toucan <- load "img/toucan.bmp"
    let out = overlap (50,50) toucan rubik
    save "tests/outputs/overlap.bmp" out

-- scale
testScale = do
    toucan <- load "img/toucan.bmp"
    let out = overlap (465,0) toucan (scale 2.5 toucan)
    save "tests/outputs/scale.bmp" out

-- rotations
testRotations = do
    lena <- load "img/lena.bmp"
    let out = lena        <|> rot90 lena
          </> rot180 lena <|> rot270 lena
    save "tests/outputs/rotations.bmp" out

-- reflections
testReflections = do
    lena <- load "img/lena.bmp"
    let out = lena <|> reflect X lena <|> reflect Y lena
    save "tests/outputs/reflections.bmp" out
