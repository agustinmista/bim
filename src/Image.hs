{-# LANGUAGE FlexibleInstances #-}

module Image (
    module Image,
    module Pixel,
    module Constants
) where

import Pixel
import Constants

type Point2D = (Int, Int)
data Dim     =   X | Y

-- Representamos un resultado como una computación que
-- puede fallar, devolviendo un String, o bien tener éxito.
type Result a = Either String a

throwError :: String -> Result a
throwError = Left

-- Representamos imágenes y operaciones primitivas sobre ellas
class Image a where
    -- Combinadores sobre imágenes
    -- Crear imagen a partir de una función
    create :: (Point2D -> Pixel) -> Point2D -> Result a
    -- Transformación de pixel en pixel
    pixelTrans :: (Point2D -> Pixel ->  Pixel) -> Result a -> Result a
    -- Transformación de vecinos en pixel, con determinado radio
    localTrans :: Int -> (a -> Pixel) -> Result a -> Result a

    -- Observaciones sobre imágenes
    -- Obtener dimensiones de una imagen (ancho x alto)
    dim :: a -> Point2D
    -- Obtener ancho (X) o alto (Y) de una imagen
    (~>) :: a -> Dim -> Int
    -- Obtener un pixel determinado (error si fuera de rango)
    (!) :: a -> Point2D -> Pixel
    -- Capacidad de consumirse mediante fold
    fold :: (Pixel -> b -> b) -> b -> a -> b
