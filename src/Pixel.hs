{-# LANGUAGE FlexibleInstances #-}

module Pixel where

type Pixel   = (Int, Int, Int)
data Channel =   R |  G |  B

-- Proyecciones de un pixel
chv :: Pixel -> Channel -> Int
(r,_,_) `chv` R = r
(_,g,_) `chv` G = g
(_,_,b) `chv` B = b

-- Hacemos un poco más sencillo trabajar con pixeles
instance Num Pixel where
    (r1,g1,b1) + (r2,g2,b2) = validate (r1+r2, g1+g2, b1+b2)
    (r1,g1,b1) * (r2,g2,b2) = validate (r1*r2, g1*g2, b1*b2)
    abs = id
    signum n = (1,1,1)
    negate (r,g,b) = (255-r, 255-g, 255-b)
    fromInteger n = ((fromInteger n) `mod` 256
                    ,(fromInteger n) `div` 256 `mod` 256
                    ,(fromInteger n) `div` 256 `div` 256 `mod` 256)

-- Validar un valor rgb de 8bits
validate :: Pixel -> Pixel
validate (r,g,b) = (aux r, aux g, aux b)
                    where aux x = min 255 (max 0 x)


-- Mapear un valor entre 0.0 ~ 1.0
($=) :: Int -> Float -> Int
n$=f = floor (fromIntegral n * f)
