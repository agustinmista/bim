{-# LANGUAGE FlexibleInstances #-}
module Operations.Point (
    (#>), thd, modify, modifyCh, mono
) where

import Image

-- Devuelve un canal de la imagen
(#>) :: Image a => Result a -> Channel -> Result a
img#>R = pixelTrans (\_ (r,_,_) -> (r,0,0)) img
img#>G = pixelTrans (\_ (_,g,_) -> (0,g,0)) img
img#>B = pixelTrans (\_ (_,_,b) -> (0,0,b)) img

-- Modificar el valor de un pixel
modify :: Image a => (Int -> Int) -> Result a -> Result a
modify f = pixelTrans (\size pix -> pixMap f pix)

-- Modificar el valor de un nivel particular
modifyCh :: Image a => Channel -> (Int -> Int) -> Result a -> Result a
modifyCh R f = pixelTrans (\size (r,g,b) -> (f r, g, b))
modifyCh G f = pixelTrans (\size (r,g,b) -> (r, f g, b))
modifyCh B f = pixelTrans (\size (r,g,b) -> (r, g, f b))

-- Convertir una imagen a monocromo
mono :: Image a => (Pixel -> Int) -> Result a -> Result a
mono f = pixelTrans (\size pix -> (f pix, f pix, f pix))

-- Threshold color mediante algún método de conversión
thd :: Image a => ConvMethod -> Float -> Result a -> Result a
thd m th = pixelTrans (\_ pix -> if m pix >= 255$=th then pix else cBLACK)

-- Threshold binario mediante algún método de conversión
bin :: Image a => ConvMethod -> Float -> Result a -> Result a
bin m th = pixelTrans (\_ pix -> if m pix >= 255$=th then cWHITE else cBLACK)
