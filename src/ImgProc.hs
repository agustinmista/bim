{-# LANGUAGE FlexibleInstances #-}

module ImgProc (
    module ImgProc,
    module Image,
    module Constants,
    module Operations.Arithmetic,
    module Operations.Point,
    module Operations.Geometric,
    module Operations.Histogram
) where

import Codec.BMP
import Data.Word
import Prelude       hiding ((!!))
import Data.Matrix   hiding ((!), (<|>))
import qualified Data.Foldable   as F
import qualified Data.ByteString as BS

import Image
import Pixel
import Constants
import Operations.Arithmetic
import Operations.Point
import Operations.Geometric
import Operations.Histogram

-- Represento mapas de bits como matrices de pixeles
type Bitmap = Matrix Pixel

instance Image Bitmap where
    create = createBitmap
    dim m = (ncols m, nrows m)
    m~>W = ncols m
    m~>H = nrows m
    m!(r,c) = getElem r c m
    pixelTrans = pixelTransBitmap
    localTrans = undefined
    fold = foldBitmap


-- Cargar una imagen desde un archivo-------------------------------------------
load :: String -> IO (Result Bitmap)
load path =
    do Right bmp <- readBMP path   -- En caso de error lanza antes una excepción
       let (w,h) = bmpDimensions bmp
       putStrLn $ "Opening " ++ path ++ " (" ++ show w ++ "x" ++ show h ++ ")"
       return $ return $ toMatrix w h bmp

toMatrix :: Int -> Int -> BMP -> Bitmap
toMatrix w h = fromList h w . toTuples . BS.foldr (:) [] . unpackBMPToRGBA32

toTuples :: [Word8] -> [Pixel]
toTuples [] = []
toTuples (r:g:b:_:xs) = (fromIntegral r, fromIntegral g, fromIntegral b) : toTuples xs
--------------------------------------------------------------------------------


-- Guardar una imagen a un archivo ---------------------------------------------
save :: String -> Result Bitmap -> IO ()
save path (Left  err) =
    putStrLn $ "Error saving image to \"" ++ path ++ "\":\n\t" ++ err
save path (Right img) =
    do let (r,c) = (nrows img, ncols img)
       putStrLn $ "Saving  " ++ path ++ " (" ++ show c ++ "x" ++ show r ++ ")"
       writeBMP path (toBMP r c img)

toBMP :: Int -> Int -> Bitmap -> BMP
toBMP h w = packRGBA32ToBMP w h . BS.pack . fromTuples . toList

fromTuples :: [Pixel] -> [Word8]
fromTuples [] = []
fromTuples ((r,g,b):xs) = [fromIntegral r, fromIntegral g, fromIntegral b, 255] ++ fromTuples xs
--------------------------------------------------------------------------------


-- Crear una imagen mediante una función ---------------------------------------
createBitmap :: (Point2D -> Pixel) -> Point2D -> Result Bitmap
createBitmap f (w,h) = return $ matrix h w f
--------------------------------------------------------------------------------

-- Transformaciónes ------------------------------------------------------------
-- De pixel en pixel
pixelTransBitmap :: (Point2D -> Pixel ->  Pixel) -> Result Bitmap -> Result Bitmap
pixelTransBitmap f img =
    do m <- img
       createBitmap (\pos -> validate (f (dim m) (m!pos))) (dim m)


-- De vecinos en pixel, con determinado radio
-- localTransBitmap :: (Point2D -> Bitmap -> Pixel) -> Int -> Result Bitmap -> Result Bitmap
-- localTransBitmap f r img =
--     do m <- img
--        createBitmap (\pos -> validate (f (dim m) (neighbours pos))) (dim m)
--             where neighbours pos = matrix (2*r+1) (2*r+1) (\npos ->
--                                         if isInside pos npos (dim m) then
--                                             )

--------------------------------------------------------------------------------

-- Fold ------------------------------------------------------------------------
foldBitmap :: (Pixel -> b -> b) -> b -> Result Bitmap -> Result b
foldBitmap f e img =
    do m <- img
       return $ F.foldr f e m
--------------------------------------------------------------------------------
