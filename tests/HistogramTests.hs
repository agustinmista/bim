import BIM

-- plotting/saving histogram
testHist = do
    baboon <- load "img/baboon.bmp"
    plotHist baboon
    saveHist (JPEG "tests/outputs/histogram.jpg") baboon
