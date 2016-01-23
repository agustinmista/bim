import BIM

-- plotting/saving histogram
testHist = do
    balloons <- load "img/balloons.bmp"
    plotHist balloons
    saveHist (JPEG "tests/outputs/histogram.jpg") balloons
