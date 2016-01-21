{-# LANGUAGE FlexibleInstances #-}

module Bitmap (
    load, save, Bitmap
) where

import Prelude     hiding (catch)
import Data.Matrix hiding ((!), (<|>))
import Control.Exception  (catch, IOException)
import Codec.BMP
import Data.Word
import qualified Data.ByteString as BS

import Image

-- Represento mapas de bits como matrices de pixeles
type Bitmap = Matrix Pixel

-- Cargar una imagen desde un archivo
load :: String -> IO (Result Bitmap)
load path = do
    putStr $ "Opening file \"" ++ path ++ "\": "
    catch (do readed <- readBMP path
              case readed of
                 Right bmp -> do
                         putStrLn "Ok"
                         let (x,y) = bmpDimensions bmp
                         return $ return $ toMatrix x bmp
                 Left err -> do
                         putStrLn "Fail"
                         return $ throw $ path ++ ": " ++ (show err))
          (\e -> do putStrLn "Fail"
                    return $ throw $ path ++ ": " ++ show (e :: IOException))

-- Guardar una imagen a un archivo
save :: String -> Result Bitmap -> IO ()
save path (Left  err) =
    putStrLn $ "Error saving image to \"" ++ path ++ "\":\t" ++ err
save path (Right img) = do
    let (x,y) = (ncols img, nrows img)
    putStrLn $ "Saving  " ++ path ++ " (" ++ show x ++ "x" ++ show y ++ ")"
    writeBMP path (toBMP y x img)


-- Divide la lista de pixeles en filas
getRows :: Int -> [Pixel] -> [[Pixel]]
getRows n [] = []
getRows n xs = let (row, rows) = splitAt n xs
                  in row : getRows n rows


-- Divide la lista de valores crudos en pixeles de 24bits
toTuples :: [Word8] -> [Pixel]
toTuples [] = []
toTuples (r:g:b:_:xs) = (fromIntegral r, fromIntegral g, fromIntegral b) : toTuples xs


-- Expande una lista de pixeles a una lista de valores crudos
toRaw :: [Pixel] -> [Word8]
toRaw [] = []
toRaw ((r,g,b):xs) = [fromIntegral r, fromIntegral g, fromIntegral b, 255] ++ toRaw xs


-- Transforma un Bitmap en un BMP
toBMP :: Int -> Int -> Bitmap -> BMP
toBMP y x img = let rows = reverse $ toLists img
                    raw = toRaw $ concat rows
                in packRGBA32ToBMP x y $ BS.pack raw


-- Transforma un BMP en una matriz con filas de tamaño n
toMatrix :: Int -> BMP -> Bitmap
toMatrix rowSize bmp = let raw = BS.foldr (:) [] $ unpackBMPToRGBA32 bmp
                           rows = getRows rowSize $ toTuples raw
                       in fromLists $ reverse rows
--------------------------------------------------------------------------------
