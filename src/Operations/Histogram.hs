module Operations.Histogram (
    saveHist, plotHist, TerminalType(..)
) where

import Prelude           hiding (catch)
import System.IO.Error   hiding (catch)
import Data.Vector       (Vector, toList, generate, accum)
import System.Info       (os)
import Control.Exception
import Graphics.EasyPlot
import System.Directory

import Image

-- Almacenamos los valores de un histograma como una tupla de coordenadas
type Hist = Vector (Float,Float)
type HistRGB = (Hist, Hist, Hist)

-- Tupla de vectores vacíos, usada como base en fold
emptyVecRGB :: HistRGB
emptyVecRGB = (emptyVec, emptyVec, emptyVec)
                where emptyVec = generate 256 (\i -> (fromIntegral i, 0))

-- Incrementa en una unidad los índices correspondientes de cada
-- vector de coordenadas
updateVecRGB :: Pixel -> HistRGB -> HistRGB
updateVecRGB (r,g,b) (rv,gv,bv) = (updateVec r rv, updateVec g gv, updateVec b bv)
                where updateVec val vec = accum (\(i,v) v' -> (i, v+v')) vec [(val, 1)]


-- Opciones de ploteo para cada canal
graphR, graphG, graphB :: [(Float, Float)] -> Graph2D Float Float
graphR = Data2D [Title "R", Style Lines, Color Red  ] [Range 0 255]
graphG = Data2D [Title "G", Style Lines, Color Green] [Range 0 255]
graphB = Data2D [Title "B", Style Lines, Color Blue ] [Range 0 255]


-- Devuelve el histograma de una imágen, hacia una ventana o archivo
histogram :: Image a => TerminalType-> Result a -> IO ()
histogram tt =
    either (\err -> putStrLn $ "Error processing histogram: " ++ err)
           (\img -> do
               let (rv,gv,bv) = fold updateVecRGB emptyVecRGB img
                   histData   = [graphR (toList rv), graphG (toList gv), graphB (toList bv)]
               plotRes <- plot tt histData
               removeTempFiles
               if plotRes
                   then putStrLn "Histogram plotted/saved succesfully"
                   else putStrLn "Error processing histogram: gnuplot returned fail exit code")

-- Guarda el histograma en un archivo
saveHist :: Image a => TerminalType -> Result a -> IO ()
saveHist tt img =
    if isFileType tt
        then histogram tt img
        else putStrLn "Error saving histogram: Invalid output file type"

-- Muestra el histograma en una ventana de gnuplot
plotHist :: Image a => Result a -> IO ()
plotHist = histogram getOSTermType


-- Checkea que saveHist se realice sobre un tipo de archivo válido
isFileType :: TerminalType -> Bool
isFileType tt = case tt of
    (PS    _) -> True
    (EPS   _) -> True
    (PNG   _) -> True
    (PDF   _) -> True
    (SVG   _) -> True
    (GIF   _) -> True
    (JPEG  _) -> True
    (Latex _) -> True
    _         -> False


-- Devuelve el tipo de terminal específico de cada plataforma, usado por gnuplot
getOSTermType :: TerminalType
getOSTermType = case os of
    "linux"   -> X11
    "darwin"  -> Aqua
    "mingw32" -> Windows


-- Remueve los archivos temporales intermedios generados por EasyPlot
removeTempFiles :: IO ()
removeTempFiles = mapM_ removeIfExists ["plot1.dat","plot2.dat","plot3.dat"]

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = catch (removeFile fileName) handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise             = throwIO e
