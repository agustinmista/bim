{-# LANGUAGE FlexibleInstances #-}

module BIM (
    module Image,
    module Bitmap,

    module Operations.Arithmetic,
    module Operations.Point,
    module Operations.Geometric,
    module Operations.Filters,
    module Operations.Histogram,
    module Operations.Palette
) where

import Data.Matrix   hiding ((!), (<|>))
import Data.Tuple    (swap)
import qualified Data.Foldable as F

import Image
import Bitmap

import Operations.Arithmetic
import Operations.Point
import Operations.Geometric
import Operations.Filters
import Operations.Histogram
import Operations.Palette

instance Image Bitmap where
    create = createBitmap
    pixelTrans = pixelTransBitmap
    localTrans = localTransBitmap
    m~>X = ncols m
    m~>Y = nrows m
    m!(x,y) = getElem y x m
    fold = F.foldr

-- Combinadores ----------------------------------------------------------------
-- Crear una imagen mediante una función
createBitmap :: (Point2D -> Pixel) -> Point2D -> Result Bitmap
createBitmap f (x,y) = return $ matrix y x (validate . f . swap)

-- De pixel en pixel
pixelTransBitmap :: (Point2D -> Pixel ->  Pixel) -> Result Bitmap -> Result Bitmap
pixelTransBitmap f img = do
    m <- img
    createBitmap (\pos -> validate (f (dim m) (m!pos))) (dim m)


-- De vecinos en pixel, con determinado radio
localTransBitmap :: Int -> (Bitmap -> Pixel) -> Result Bitmap -> Result Bitmap
localTransBitmap r f img =
    do m <- img
       padded <- extend r img
       createBitmap (\pos -> validate (f (neighbours pos padded))) (dim m)
            where neighbours (x,y) m = submatrix y (y+2*r) x (x+2*r) m

-- Extiende los bordes de una imagen (padding), con determinado radio
extend :: Int -> Result Bitmap -> Result Bitmap
extend r img = do
    m <- img
    createBitmap (fun m) (m~>X + 2*r, m~>Y + 2*r)
    where fun m (x,y) | x <= r       && y <= r       = m!(1, 1)       -- Esq. sup. izquierda
                      | x <= r       && y > m~>Y + r = m!(1, m~>Y)    -- Esq. inf. izquierda
                      | x > m~>X + r && y <= r       = m!(m~>X, 1)    -- Esq. sup. derecha
                      | x > m~>X + r && y > m~>Y + r = m!(m~>X, m~>Y) -- Esq. inf. derecha
                      | x <= r                       = m!(1, y-r)     -- Borde izquierdo
                      | x > m~>X + r                 = m!(m~>X, y-r)  -- Borde derecho
                      | y <= r                       = m!(x-r, 1)     -- Borde superior
                      | y > m~>Y + r                 = m!(x-r, m~>Y)  -- Borde inferior
                      | otherwise                    = m!(x-r, y-r)   -- La imagen en si
