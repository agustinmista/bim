{-# LANGUAGE FlexibleInstances #-}
module Operations.Filters (
    mean, median, convolution, sobel, sharpen, emboss, edgeDetect, blur
) where

import Data.List (sortBy)

import Image
import Operations.Arithmetic

-- Filtro de promedio, de radio r
mean :: Image a => Int -> Result a -> Result a
mean r = localTrans r (fold sumf cBLACK)
            where sumf (r,g,b) (rs, gs, bs) = (r$=v + rs, g$=v + gs, b$=v + bs)
                  v = 1 / (fromIntegral ((2*r+1)^2))

-- Filtro de mediana, de radio r
median :: Image a => Int -> Result a -> Result a
median r = localTrans r (getMid . sortBy lumOrd . fold (:) [])
            where getMid xs = xs !! ((2*r+1)^2 `div` 2)


-- Convolution kernels
convolution :: Image a => [Int] -> Result a -> Result a
convolution k = localTrans 1 (zipKernel k . fold (:) [])
            where zipKernel k = sum . zipWith zipper k
                  zipper i pix = pixMap (*i) pix

-- Deteccion de bordes de sobel
sobel :: Image a => Result a -> Result a
sobel img = (convolution sobelXKernel img) + (convolution sobelYKernel img)

sobelXKernel = [-1,  0,  1,
                -2,  0,  2,
                -1,  0,  1 ]

sobelYKernel = [ 1,  2,  1,
                 0,  0,  0,
                -1, -2, -1 ]

-- Deteccion de bordes simple
edgeDetect :: Image a => Result a -> Result a
edgeDetect = convolution edgeDetectKernel

edgeDetectKernel = [ 0,  1,  0,
                     1, -4,  1,
                     0,  1,  0 ]

-- Desenfoque
blur :: Image a => Result a -> Result a
blur = convolution blurKernel

blurKernel = [ 1,  1,  1,
               1,  1,  1,
               1,  1,  1 ]

-- Enfoque
sharpen :: Image a => Result a -> Result a
sharpen = convolution sharpenKernel

sharpenKernel = [ 0, -1,  0,
                 -1,  5, -1,
                  0, -1,  0 ]

-- Efecto de grabado
emboss :: Image a => Result a -> Result a
emboss = convolution embossKernel

embossKernel = [ -2, -1,  0,
                 -1,  1,  1,
                  0,  1,  2 ]
