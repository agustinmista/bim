module Operations.Point (
    modify, modifyCh, (#>),
    negative, mono,
    threshold, binary,
) where

import Image

-- Modificar el valor de un pixel mediante una función
modify :: Image a => (Int -> Int) -> Result a -> Result a
modify f = pixelTrans (\size -> pixelMap f)

-- Modificar el valor de un nivel particular mediante una función
modifyCh :: Image a => Channel -> (Int -> Int) -> Result a -> Result a
modifyCh R f = pixelTrans (\size (r,g,b) -> (f r, g, b))
modifyCh G f = pixelTrans (\size (r,g,b) -> (r, f g, b))
modifyCh B f = pixelTrans (\size (r,g,b) -> (r, g, f b))

-- Devuelve un canal de la imagen
(#>) :: Image a => Result a -> Channel -> Result a
img#>R = pixelTrans (\size (r,_,_) -> (r,0,0)) img
img#>G = pixelTrans (\size (_,g,_) -> (0,g,0)) img
img#>B = pixelTrans (\size (_,_,b) -> (0,0,b)) img

-- Negativo de una imagen
negative :: Image a => Result a -> Result a
negative = modify ((-) 255)

-- Convertir una imagen a monocromo, con determinado método
mono :: Image a => ConvMethod -> Result a -> Result a
mono m = pixelTrans (\size pix -> (m pix, m pix, m pix))

-- Threshold color y binario, mediante algún método de conversión
threshold :: Image a => ConvMethod -> Float -> Result a -> Result a
threshold m th img
        | th > 1.0 || th < 0.0 = throwError "threshold: threshold must be between 0.0 ~ 1.0"
        | otherwise            = pixelTrans (\size pix -> if m pix >= 255%>th then pix else black) img

binary :: Image a => ConvMethod -> Float -> Result a -> Result a
binary m th img
        | th > 1.0 || th < 0.0 = throwError "binary: threshold must be between 0.0 ~ 1.0"
        | otherwise            = pixelTrans (\size pix -> if m pix >= 255%>th then white else black) img
