{-# LANGUAGE FlexibleInstances #-}

module BIM (
    module BIM,
    module Image,
    module ImageIO,
    module Constants,
    module Operations.Arithmetic,
    module Operations.Point,
    module Operations.Geometric,
    module Operations.Histogram
) where

import Prelude       hiding ((!!), catch)
import Data.Matrix   hiding ((!), (<|>))
import Data.Tuple    (swap)
import qualified Data.Foldable as F


import Image
import ImageIO
import Pixel
import Constants
import Operations.Arithmetic
import Operations.Point
import Operations.Geometric
import Operations.Histogram

-- Represento mapas de bits como matrices de pixeles
type Bitmap = Matrix Pixel

instance Image Bitmap where
    create = createBitmap
    dim m = (ncols m, nrows m)
    m~>X = ncols m
    m~>Y = nrows m
    m!(x,y) = getElem y x m
    pixelTrans = pixelTransBitmap
    localTrans = undefined
    fold = foldBitmap


-- Crear una imagen mediante una función
createBitmap :: (Point2D -> Pixel) -> Point2D -> Result Bitmap
createBitmap f (x,y) = return $ matrix y x (f . swap)

-- Transformaciónes
-- De pixel en pixel
pixelTransBitmap :: (Point2D -> Pixel ->  Pixel) -> Result Bitmap -> Result Bitmap
pixelTransBitmap f img = do
    m <- img
    createBitmap (\pos -> validate (f (dim m) (m!pos))) (dim m)


-- De vecinos en pixel, con determinado radio
localTransBitmap :: (Point2D -> Bitmap -> Pixel) -> Int -> Result Bitmap -> Result Bitmap
localTransBitmap f r img =
    do m <- img
       padded <- extend r img
       createBitmap (\pos -> validate (f (dim m) (neighbours pos padded))) (dim m)
            where neighbours (x,y) = submatrix (y-r) (y+r) (x-r) (x+r)

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




-- Fold
foldBitmap :: (Pixel -> b -> b) -> b -> Result Bitmap -> Result b
foldBitmap f e img = do
    m <- img
    return $ F.foldr f e m
