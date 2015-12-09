{-# LANGUAGE FlexibleInstances #-}
module Operations.Geometric (
    refH, refV, rot90, rot180, rot270, (<|>), (</>)
) where

import Image
import Proc

-- Reflexiones
refH :: Image a => Proc a -> Proc a
refH img = do
    m <- img
    create (\(row,col) -> m!(m~>H - row + 1, col)) (dim m)

refV :: Image a => Proc a -> Proc a
refV img = do
    m <- img
    create (\(row,col) -> m!(row, m~>W - col + 1)) (dim m)

-- Rotaciones (sentido horario)
rot90 :: Image a => Proc a -> Proc a
rot90 img = do
    m <- img
    create (\(row,col) -> m!(col, m~>H - row + 1)) (m~>H, m~>W)

rot180 :: Image a => Proc a -> Proc a
rot180 img = do
    m <- img
    create (\(row,col) -> m!(m~>H - row + 1, m~>W - col + 1)) (m~>W, m~>H)

rot270 :: Image a => Proc a -> Proc a
rot270 img = do
    m <- img
    create (\(row,col) -> m!(m~>H - col + 1, row)) (m~>H, m~>W)

-- Une dos imágenes, una al lado de la otra
infix 4 <|>
(<|>) :: Image a => Proc a -> Proc a -> Proc a
img1 <|> img2 =
    do left  <- img1
       right <- img2
       if left~>H /= right~>H
          then failProc "(<|>) : sizes do not match"
          else let f (row,col) = if col <= left~>W
                                 then left !(row, col)
                                 else right!(row, col - left~>W)
               in create f (left~>W + right~>W, left~>H)

-- Une dos imágenes, una encima de la otra
infix 3 </>
(</>) :: Image a => Proc a -> Proc a -> Proc a
img1 </> img2 =
    do upper <- img2    -- Ésto es raro, parece que las imágenes quedan
       lower <- img1    -- intercambiadas si lo hago de la manera intuitiva
       if upper~>W /= lower~>W
           then failProc "(</>) : sizes do not match"
           else let f (row,col) = if row <= upper~>H
                                  then upper!(row, col)
                                  else lower!(row - upper~>H, col)
                 in create f (upper~>W, upper~>H + lower~>H)
