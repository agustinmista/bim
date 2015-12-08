{-# LANGUAGE FlexibleInstances #-}
module PointOperators where

import Image
import Proc

-- Devuelve un canal de la imagen
(#>) :: Image a => Proc a -> Channel -> Proc a
img#>R = pixelTrans (\size (r,_,_) -> (r,0,0)) img
img#>G = pixelTrans (\size (_,g,_) -> (0,g,0)) img
img#>B = pixelTrans (\size (_,_,b) -> (0,0,b)) img

-- Threshold con valor promedio de los canales
thd_avg :: Image a => Float -> Proc a -> Proc a
thd_avg th img = pixelTrans (aux th) img
                where aux th size (r,g,b) =
                        if (r+g+b) `div` 3 >= 255$=th
                        then (r,g,b)
                        else cBLACK

-- Threshold sobre un canal
thd :: Image a => Channel -> Float -> Proc a -> Proc a
thd ch th = pixelTrans (\size pix -> if (pix `chv` ch >= 255$=th)
                                     then pix
                                     else cBLACK)

-- Modificar el brillo
bright :: Image a => (Int->Int) -> Proc a -> Proc a
bright f = pixelTrans (\size (r,g,b) -> (f r, f g, f b))

-- Modificar un nivel
lvl :: Image a => Channel -> (Int->Int) -> Proc a -> Proc a
lvl R f = pixelTrans (\size (r,g,b) -> (f r, g, b))
lvl G f = pixelTrans (\size (r,g,b) -> (r, f g, b))
lvl B f = pixelTrans (\size (r,g,b) -> (r, g, f b))
