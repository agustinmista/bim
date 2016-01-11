{-# LANGUAGE FlexibleInstances #-}
module Operations.Histogram (
    hist
) where

import Data.Vector
import Graphics.EasyPlot
import System.Info

import Image

emptyVectors :: (Vector (Float, Float), Vector (Float, Float), Vector (Float, Float))
emptyVectors = (generate 256 (\i -> (fromIntegral i, 0)), generate 256 (\i -> (fromIntegral i, 0)), generate 256 (\i -> (fromIntegral i, 0)))

plus1 vec val = accum (\(i,v) v' -> (i, v+v')) vec [(val, 1)]

updateVectors (r,g,b) (rv,gv,bv) = (plus1 rv r, plus1 gv g, plus1 bv b)

hist :: Image a => Result a -> IO Bool
hist img = do
    let termType = case os of
                      "linux" -> X11
    case fold updateVectors emptyVectors img of
        Right (rv, gv, bv) ->  do
            plot termType
                [Data2D [Title "R", Style Lines, Color Red  ] [Range 0 255]  (toList rv),
                 Data2D [Title "G", Style Lines, Color Green] [Range 0 255]  (toList gv),
                 Data2D [Title "B", Style Lines, Color Blue ] [Range 0 255]  (toList bv)]
            return True
        Left err -> do putStrLn err
                       return False
