{-# LANGUAGE FlexibleInstances #-}
module Arithmetic where

import Image
import Proc

-- Doy instancias para operaciones aritméticas entre imágenes
instance Image a => Num (Proc a) where
    img1 + img2 =
        do m <- img1
           n <- img2
           if dim m /= dim n
              then failProc "(+) : sizes do not match"
              else create (\pos -> (m!pos + n!pos)) (dim m)
    img1 * img2 =
        do m <- img1
           n <- img2
           if dim m /= dim n
              then failProc "(*) : sizes do not match"
              else create (\pos -> (m!pos * n!pos)) (dim m)
    abs = id
    signum img =
        do m <- img
           create (\_-> (1,1,1)) (dim m)
    negate = pixelTrans (\size pix -> negate pix)
    fromInteger = undefined


-- Opacidad entre 0.0 ~ 1-0
(*=) :: Image a => Proc a -> Float -> Proc a
img*=a = if a > 1.0 || a < 0.0
             then failProc "(@>) : alpha must be between 0.0 ~ 1.0"
             else pixelTrans (\size (r, g, b) -> (r$=a, g$=a, b$=a)) img


-- Mezcla dos imágenes con una proporcion lineal entre 0.0 ~ 1-0
blend :: Image a => Float -> Proc a -> Proc a -> Proc a
blend a img1 img2 = img1*=a + img2*=(1-a)
