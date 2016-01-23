import BIM

-- dilation/erosion
testDilationErotion = do
    toucan <- load "img/toucan.bmp"
    mask   <- load "img/mask.bmp"
    let maskOut   = erosion 10 mask  <|> mask   <|> dilation 10 mask
        toucanOut = erosion 1 toucan <|> toucan <|> dilation 1 toucan
    save "tests/outputs/dilationErotionMask.bmp"   maskOut
    save "tests/outputs/dilationErotionToucan.bmp" toucanOut

-- mean/median
testMeanMeadian = do
    noisy <- load "img/noisy.bmp"
    let out = noisy <|> mean 1 noisy <|> median 1 noisy
    save "tests/outputs/meanMedian.bmp" out

-- edgesDetection
testEdgeDetection = do
    rubik <- load "img/rubik.bmp"
    let out = rubik <|> sobel rubik <|> laplace rubik
    save "tests/outputs/edgeDetection.bmp" out

-- blurs
testBlurs = do
    toucan <- load "img/toucan.bmp"
    let out = toucan          <|> smooth toucan
                              </>
              gaussian toucan <|> motionBlur A toucan
    save "tests/outputs/blurs.bmp" out

-- sharpen
testSharpen = do
    toucan <- load "img/toucan.bmp"
    save "tests/outputs/sharpen.bmp" (toucan </> sharpen toucan)

-- emboss
testEmboss = do
    tools <- load "img/tools.bmp"
    save "tests/outputs/emboss.bmp" (tools </> emboss tools)
