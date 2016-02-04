module Operations.Palette (
    palette, dominants
) where

import Data.Map (fromListWith, toList)
import Data.List (sortBy)

import Image

-- A partir de una imagen, crea otra de determinado tamaño con los n colores
-- más dominantes de la primera
palette :: Image a => Point2D -> Int -> Result a -> Result a
palette size@(w,h) n img = do
        m <- img
        let colors = dominants n m
            tileSize = (w `div` (min n (length colors))) + 1
        create (\(x,_) -> colors !! (x `div` tileSize)) size


-- Obtiene los n colores más dominantes de una imágen
dominants :: Image a => Int -> a -> [Pixel]
dominants n = map fst . take n . sortDecBySnd . frequency . to256ColorsList

sortDecBySnd :: Ord b => [(a,b)] -> [(a,b)]
sortDecBySnd = sortBy (\x y -> snd y `compare` snd x)

to256ColorsList :: Image a => a -> [Pixel]
to256ColorsList = fold (\p ps -> to256 p : ps) []
                where to256 = pixelMap (\v -> (div v  32) * 32)

-- Cuenta la frecuencia de aparición de un elemento en una lista
-- http://stackoverflow.com/questions/10398698/haskell-counting-how-many-times-each-distinct-element-in-a-list-occurs
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])
