module Image (
    module Image,
    module Pixel,
    module Colors,
    throwError
) where

import Control.Monad.Except (throwError)

import Pixel
import Colors

-- Representamos un resultado como una computación que
-- puede fallar, devolviendo un String, o bien tener éxito
type Result a = Either String a

-- Ancho y alto de una imagen
data Dim = X | Y

-- Posiciones en el espacio
type Point2D = (Int, Int)

-- Atajo para obtener dimensiones de una imagen
dim :: Image a => a -> (Int, Int)
dim img = (img~>X, img~>Y)

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
    -- Obtener ancho (X) o alto (Y) de una imagen
    (~>) :: a -> Dim -> Int
    -- Obtener un pixel determinado (error si fuera de rango)
    (!) :: a -> Point2D -> Pixel
    -- Capacidad de consumirse mediante fold
    fold :: (Pixel -> b -> b) -> b -> a -> b
