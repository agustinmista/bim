{-# LANGUAGE FlexibleInstances #-}

module ImgProc (
    module ImgProc,
    module Image,
    module Proc,
    module Constants,
    module Arithmetic,
    module PointOperators,
    module Geometric
)where

import Codec.BMP
import Data.Word
import Prelude     hiding ((!!))
import Data.Matrix hiding ((!), (<|>))
import Control.Monad.IO.Class
import qualified Data.ByteString as BS

import Image
import Proc
import Pixel
import Constants
import Arithmetic
import PointOperators
import Geometric

-- Represento imágenes como matrices de pixeles
type MatImg = Matrix Pixel

instance Image MatImg where
    load = loadMatrix
    save = saveMatrix
    create = createMatrix
    dim m = (ncols m, nrows m)
    m~>W = ncols m
    m~>H = nrows m
    m!(r,c) = getElem r c m
    pixelTrans = pixelTransMatrix
    localTrans = undefined
    fold = undefined



-- Cargar una imagen desde un archivo-------------------------------------------
loadMatrix :: String -> Proc MatImg
loadMatrix path = liftIO $
    do Right bmp <- readBMP path   -- En caso de error lanza antes una excepción
       let (w,h) = bmpDimensions bmp
       putStrLn $ "Opening " ++ path ++ " (" ++ show w ++ "x" ++ show h ++ ")"
       return $ toMatrix w h bmp

toMatrix :: Int -> Int -> BMP -> MatImg
toMatrix w h = fromList h w . toTuples . BS.foldr (:) [] . unpackBMPToRGBA32

toTuples :: [Word8] -> [Pixel]
toTuples [] = []
toTuples (r:g:b:_:xs) = (fromIntegral r, fromIntegral g, fromIntegral b) : toTuples xs
--------------------------------------------------------------------------------


-- Guardar una imagen a un archivo ---------------------------------------------
saveMatrix :: Proc MatImg -> String -> Proc ()
saveMatrix img path =
    do m <- img
       (liftIO $
          do let (r,c) = (nrows m, ncols m)
             putStrLn $ "Saving  " ++ path ++ " (" ++ show c ++ "x" ++ show r ++ ")"
             writeBMP path (toBMP r c m))

toBMP :: Int -> Int -> MatImg -> BMP
toBMP h w = packRGBA32ToBMP w h . BS.pack . fromTuples . toList

fromTuples :: [Pixel] -> [Word8]
fromTuples [] = []
fromTuples ((r,g,b):xs) = [fromIntegral r, fromIntegral g, fromIntegral b, 255] ++ fromTuples xs
--------------------------------------------------------------------------------


-- Crear una imagen mediante una función ---------------------------------------
createMatrix :: (Point2D -> Pixel) -> Point2D -> Proc MatImg
createMatrix f (w,h) = liftIO $
       do putStrLn $ "Creating image (" ++ show w ++ "x" ++ show h ++ ")"
          return $ matrix h w f
--------------------------------------------------------------------------------

-- Transformaciónes ------------------------------------------------------------
-- De pixel en pixel
pixelTransMatrix :: (Point2D -> Pixel ->  Pixel) -> Proc MatImg -> Proc MatImg
pixelTransMatrix f img =
    do m <- img
       createMatrix (\pos-> validate (f (dim m) (m!pos))) (dim m)
--------------------------------------------------------------------------------
