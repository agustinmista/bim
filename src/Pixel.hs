{-# LANGUAGE FlexibleInstances #-}

module Pixel (
    module Pixel
) where

-- Representamos pixeles mediante tuplas de valores RGB
type Pixel = (Int, Int, Int)
data Channel = R | G | B

-- Hacemos un poco más sencillo trabajar con pixeles
instance Num Pixel where
    (r1,g1,b1) + (r2,g2,b2) = (r1+r2, g1+g2, b1+b2)
    (r1,g1,b1) * (r2,g2,b2) = (r1*r2, g1*g2, b1*b2)
    abs = id
    signum n = (1,1,1)
    negate (r,g,b) = (-r,-g,-b)
    fromInteger n = ((fromInteger n) `div` 256 `div` 256 `mod` 256
                    ,(fromInteger n) `div` 256 `mod` 256
                    ,(fromInteger n) `mod` 256)

-- Métodos de conversión RGB -> B&W
type ConvMethod = (Pixel -> Int)

lig, lum, avg :: ConvMethod
lig (r,g,b) = (maximum [r,g,b] + minimum [r,g,b]) `div` 2
lum (r,g,b) = r%>0.21 + g%>0.72 + b%>0.07
avg (r,g,b) = (r+g+b) `div` 3

-- Hacemos pixeles ordenables convirtiéndolos a enteros mediante luminancia
lumOrd :: Pixel -> Pixel -> Ordering
p1 `lumOrd` p2 = lum p1 `compare` lum p2

-- Validar un valor rgb de 8bits
validate :: Pixel -> Pixel
validate (r,g,b) = (aux r, aux g, aux b)
                    where aux x = min 255 (max 0 x)

-- Mapear una función sobre un pixel
pixelMap :: (Int -> Int) -> Pixel -> Pixel
pixelMap f (r,g,b) = (f r, f g, f b)

-- Multiplica un valor entero por una constante real
(%>) :: Int -> Float -> Int
n%>f = floor (fromIntegral n * f)
