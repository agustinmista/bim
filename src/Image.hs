{-# LANGUAGE FlexibleInstances #-}

module Image (
    module Image,
    module Pixel,
    module Constants
) where

import Pixel
import Proc
import Constants
import Data.Foldable

type Point2D = (Int, Int)
data Dim     = W | H

-- Representamos imágenes y operaciones primitivas sobre ellas
class Image a where
    -- Cargar una imagen desde un archivo
    load :: String -> Proc a
    -- Guardar una imagen a un archivo
    save :: Proc a -> String -> Proc ()
    -- Crear una imagen mediante una función
    create :: (Point2D -> Pixel) -> Point2D -> Proc a
    -- Obtener dimensiones de una imagen
    dim :: a -> Point2D
    -- Obtener alto (H) o ancho (W) de una imagen
    (~>) :: a -> Dim -> Int
    -- Obtener un pixel determinado (error si fuera de rango)
    (!) :: a -> Point2D -> Pixel
    -- Transformación de pixel en pixel
    pixelTrans :: (Point2D -> Pixel ->  Pixel) -> Proc a -> Proc a
    -- Transformación de vecinos en pixel, con determinado radio
    localTrans :: (a -> Point2D -> Pixel) -> Int -> Proc a -> Proc a
    -- Capacidad de consumirse mediante fold
    fold :: (Pixel -> b -> b) -> Proc a -> b
