{-# LANGUAGE FlexibleInstances #-}
module Operations.Palette (
    dominantColors, palette
) where

import Data.Map (fromListWith, toList)
import Data.List (sortBy)

import Image

-- Crea una imagen de tamaño WxH con los n colores más dominantes de una imágen
palette :: Image a => Point2D -> Int -> Result a -> Result a
palette size@(w,h) n img = do
        m <- img
        let dominants = dominantColors n m
            tileSize = (w `div` (min n (length dominants))) + 1
        create (\(x,_) -> dominants !! (x `div` tileSize)) size


-- Obtiene los n colores más dominantes de una imágen
dominantColors :: Image a => Int -> a -> [Pixel]
dominantColors n = map fst . take n . sortDecBySnd . frequency . to256ColorsList

sortDecBySnd :: Ord b => [(a,b)] -> [(a,b)]
sortDecBySnd = sortBy (\x y -> snd y `compare` snd x)

to256ColorsList :: Image a => a -> [Pixel]
to256ColorsList = fold (\p ps -> to256 p : ps) []
                where to256 = pixelMap (\v -> (div v  32) * 32)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])
