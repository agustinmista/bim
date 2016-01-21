{-# LANGUAGE FlexibleInstances #-}
module Operations.Filters (
    mean, median, convolution, sobel,
    sharpen, emboss, laplace, smooth,
    gaussian, erosion, dilation, motionBlur, Orientation(..)
) where

import Data.List (sortBy)

import Image
import Operations.Arithmetic

-- Erosion, de radio r
erosion :: Image a => Int -> Result a -> Result a
erosion r = localTrans r (fold min white)

-- Dilation, de radio r
dilation :: Image a => Int -> Result a -> Result a
dilation r = localTrans r (fold max black)


-- Filtro de promedio, de radio r
mean :: Image a => Int -> Result a -> Result a
mean r = localTrans r (fold sumf black)
            where sumf (r,g,b) (rs, gs, bs) = (r*>v + rs, g*>v + gs, b*>v + bs)
                  v = 1 / (fromIntegral ((2*r+1)^2))

-- Filtro de mediana, de radio r
median :: Image a => Int -> Result a -> Result a
median r = localTrans r (getMid . sortBy lumOrd . fold (:) [])
            where getMid xs = xs !! ((2*r+1)^2 `div` 2)


-- Convolution kernels
convolution :: Image a => Int -> [Float] -> Result a -> Result a
convolution r k img
        | length k /= (2*r+1)^2 = throw "convolution: kernel size don't match radius"
        | otherwise             = localTrans 1 (zipKernel k . fold (:) []) img
                                    where zipKernel k = sum . zipWith zipper k
                                          zipper i pix = pixelMap (*>i) pix

-- Deteccion de bordes de sobel
sobel :: Image a => Result a -> Result a
sobel img = (convolution 1 sobelXKernel img) <+> (convolution 1 sobelYKernel img)

sobelXKernel = [-1,  0,  1,
                -2,  0,  2,
                -1,  0,  1 ]

sobelYKernel = [ 1,  2,  1,
                 0,  0,  0,
                -1, -2, -1 ]

-- Deteccion de bordes de laplace
laplace :: Image a => Result a -> Result a
laplace = convolution 1 laplaceKernel

laplaceKernel = [ 0, -1,  0,
                 -1,  4, -1,
                  0, -1,  0 ]

-- Desenfoque suave
smooth :: Image a => Result a -> Result a
smooth = convolution 1 smoothKernel

smoothKernel = [   0, 1/5,   0,
                 1/5, 1/5, 1/5,
                   0, 1/5,   0 ]

-- Desenfoque gaussiano
gaussian :: Image a => Result a -> Result a
gaussian = convolution 1 gaussianKernel

gaussianKernel = [ 1/16,  1/8,  1/16,
                   1/8,   1/4,  1/8,
                   1/16,  1/8,  1/16 ]

-- Desenfoque de movimiento
data Orientation = H | V | A | D

motionBlur :: Image a => Orientation -> Result a -> Result a
motionBlur H = convolution 1 hmbKernel
motionBlur V = convolution 1 vmbKernel
motionBlur A = convolution 1 ambKernel
motionBlur D = convolution 1 dmbKernel

hmbKernel = [   0,   0,    0,
              1/3, 1/3,  1/3,
                0,   0,    0 ]

vmbKernel = [ 0, 1/3, 0,
              0, 1/3, 0,
              0, 1/3, 0 ]

ambKernel = [   0,   0, 1/3,
                0, 1/3,   0,
              1/3,   0,   0 ]

dmbKernel = [ 1/3,   0,   0,
                0, 1/3,   0,
                0,   0, 1/3 ]

-- Enfoque
sharpen :: Image a => Result a -> Result a
sharpen = convolution 1 sharpenKernel

sharpenKernel = [ 0, -1,  0,
                 -1,  5, -1,
                  0, -1,  0 ]

-- Efecto de grabado
emboss :: Image a => Result a -> Result a
emboss = convolution 1 embossKernel

embossKernel = [ -2, -1,  0,
                 -1,  1,  1,
                  0,  1,  2 ]
