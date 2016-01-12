{-# LANGUAGE FlexibleInstances #-}
module Operations.Histogram (
    histogram, saveHist, plotHist, TerminalType(..)
) where

import Data.Vector (Vector, toList, generate, accum)
import Graphics.EasyPlot
import System.Info

import Image

-- Almacenamos los valores de un histograma como una tupla de coordenadas
type Hist = Vector (Float,Float)
type HistRGB = (Hist, Hist, Hist)

emptyVec :: Hist
emptyVec = generate 256 (\i -> (fromIntegral i, 0))

emptyVecRGB :: HistRGB
emptyVecRGB = (emptyVec, emptyVec, emptyVec)


updateVec :: Int -> Hist -> Hist
updateVec val vec = accum (\(i,v) v' -> (i, v+v')) vec [(val, 1)]

updateVecRGB :: Pixel -> HistRGB -> HistRGB
updateVecRGB (r,g,b) (rv,gv,bv) = (updateVec r rv, updateVec g gv, updateVec b bv)

graphR, graphG, graphB :: [(Float, Float)] -> Graph2D Float Float
graphR = Data2D [Title "R", Style Lines, Color Red  ] [Range 0 255]
graphG = Data2D [Title "G", Style Lines, Color Green] [Range 0 255]
graphB = Data2D [Title "B", Style Lines, Color Blue ] [Range 0 255]


-- Guarda el histograma en un archivo
saveHist :: Image a => TerminalType -> Result a -> IO ()
saveHist tt img = if isFileType tt
                then histogram tt img
                else putStrLn "Error saving histogram: Invalid output file type"

plotHist :: Image a => Result a -> IO ()
plotHist = histogram getTermType

histogram :: Image a => TerminalType-> Result a -> IO ()
histogram tt img = do
    case fold updateVecRGB emptyVecRGB img of
        Left err -> do putStrLn $ "Error plotting histogram: " ++ err
        Right (rv,gv,bv) -> do
            let histData = [graphR (toList rv), graphG (toList gv), graphB (toList bv)]
            plotRes <- plot tt histData
            if plotRes
                then putStrLn "Histogram plotted succesfully"
                else putStrLn "Error calling gnuplot"

------- VER!
isFileType tt = True

getTermType = case os of
                "linux"   -> X11
                "darwin"  -> Aqua
                "mingw32" -> Windows
