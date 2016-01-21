{-# LANGUAGE FlexibleInstances #-}
module Operations.Arithmetic (
    (*=), blend, (<+>), (<->), (<*>)
) where

import Image

-- Suma dos imágenes
(<+>) :: Image a => Result a -> Result a -> Result a
img1 <+> img2 = do
     m <- img1
     n <- img2
     if dim m /= dim n
        then throw "(+) : sizes do not match"
        else create (\pos -> (m!pos + n!pos)) (dim m)

-- Resta dos imágenes
(<->) :: Image a => Result a -> Result a -> Result a
img1 <-> img2 = do
     m <- img1
     n <- img2
     if dim m /= dim n
        then throw "(-) : sizes do not match"
        else create (\pos -> (m!pos - n!pos)) (dim m)

-- Multiplica dos imágenes
(<*>) :: Image a => Result a -> Result a -> Result a
img1 <*> img2 = do
     m <- img1
     n <- img2
     if dim m /= dim n
        then throw "(*) : sizes do not match"
        else create (\pos -> (m!pos * n!pos)) (dim m)

-- Multiplica una imagen por una constante
(*=) :: Image a => Result a -> Float -> Result a
img*=a = pixelTrans (\size -> pixelMap (*>a)) img

-- Mezcla dos imágenes con una proporcion lineal entre 0.0 ~ 1-0
blend :: Image a => Float -> Result a -> Result a -> Result a
blend a img1 img2
        | a > 1.0 || a < 0.0 = throw "blend: alpha must be between 0.0 ~ 1.0"
        | otherwise          = img1*=a <+> img2*=(1-a)
