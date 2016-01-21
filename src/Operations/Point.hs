{-# LANGUAGE FlexibleInstances #-}
module Operations.Point (
    (#>), thd, modify, modifyCh, mono, bin, negative
) where

import Image

-- Devuelve un canal de la imagen
(#>) :: Image a => Result a -> Channel -> Result a
img#>R = pixelTrans (\size (r,_,_) -> (r,0,0)) img
img#>G = pixelTrans (\size (_,g,_) -> (0,g,0)) img
img#>B = pixelTrans (\size (_,_,b) -> (0,0,b)) img

-- Modificar el valor de un pixel mediante una función
modify :: Image a => (Int -> Int) -> Result a -> Result a
modify f = pixelTrans (\size-> pixelMap f)

-- Modificar el valor de un nivel particular mediante una función
modifyCh :: Image a => Channel -> (Int -> Int) -> Result a -> Result a
modifyCh R f = pixelTrans (\size (r,g,b) -> (f r, g, b))
modifyCh G f = pixelTrans (\size (r,g,b) -> (r, f g, b))
modifyCh B f = pixelTrans (\size (r,g,b) -> (r, g, f b))

-- Convertir una imagen a monocromo, con determinado método
mono :: Image a => ConvMethod -> Result a -> Result a
mono m = pixelTrans (\size pix -> (m pix, m pix, m pix))

-- Negativo de una imagen
negative :: Image a => Result a -> Result a
negative = modify ((-)255)

-- Threshold color mediante algún método de conversión
thd :: Image a => ConvMethod -> Float -> Result a -> Result a
thd m th img
        | th > 1.0 || th < 0.0 = throw "thd: threshold must be between 0.0 ~ 1.0"
        | otherwise            = pixelTrans (\size pix -> if m pix >= 255*>th then pix else black) img

-- Threshold binario mediante algún método de conversión
bin :: Image a => ConvMethod -> Float -> Result a -> Result a
bin m th img
        | th > 1.0 || th < 0.0 = throw "bin: threshold must be between 0.0 ~ 1.0"
        | otherwise            = pixelTrans (\size pix -> if m pix >= 255*>th then white else black) img
