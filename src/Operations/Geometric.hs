{-# LANGUAGE FlexibleInstances #-}
module Operations.Geometric (
    refH, refV, rot90, rot180, rot270, (<|>), (</>)
) where

import Image

-- Reflexiones
refH :: Image a => Result a -> Result a
refH img =
    do m <- img
       create (\(x,y) -> m!(x, m~>Y - y + 1)) (dim m)

refV :: Image a => Result a -> Result a
refV img =
    do m <- img
       create (\(x,y) -> m!(m~>X - x + 1, y)) (dim m)

-- Rotaciones (sentido horario)
rot90 :: Image a => Result a -> Result a
rot90 img =
    do m <- img
       create (\(x,y) -> m!(m~>X - y + 1, x)) (m~>Y, m~>X)

rot180 :: Image a => Result a -> Result a
rot180 img =
    do m <- img
       create (\(x,y) -> m!(m~>X - x + 1, m~>Y - y + 1)) (m~>X, m~>Y)

rot270 :: Image a => Result a -> Result a
rot270 img =
    do m <- img
       create (\(x,y) -> m!(y, m~>Y - x + 1)) (m~>Y, m~>X)

-- Une dos imágenes, una al lado de la otra
infixl 4 <|>
(<|>) :: Image a => Result a -> Result a -> Result a
img1 <|> img2 =
    do left  <- img1
       right <- img2
       if left~>Y /= right~>Y
          then throwError "(<|>) : sizes do not match"
          else let f (x,y) = if x <= left~>X
                                 then left !(x, y)
                                 else right!(x - left~>X, y)
               in create f (left~>X + right~>X, left~>Y)

-- Une dos imágenes, una encima de la otra
infixl 3 </>
(</>) :: Image a => Result a -> Result a -> Result a
img1 </> img2 =
    do upper <- img1
       lower <- img2
       if upper~>X /= lower~>X
           then throwError "(</>) : sizes do not match"
           else let f (x,y) = if y <= upper~>Y
                                  then upper!(x, y)
                                  else lower!(x, y - upper~>Y)
                 in create f (upper~>X, upper~>Y + lower~>Y)
