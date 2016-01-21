{-# LANGUAGE FlexibleInstances #-}
module Operations.Geometric (
    refH, refV,
    rot90, rot180, rot270,
    (<|>), (</>), crop, scale, overlap
) where

import Image

-- Escalado
scale :: Image a => (Float, Float) -> Result a -> Result a
scale = undefined

-- Superposición de una imagen sobre otra, resultando en el tamaño de la segunda
overlap :: Image a => Point2D -> Result a -> Result a -> Result a
overlap (x,y) img1 img2 = do
    front <- img1
    back <- img2
    let xstart = min (back~>X) (max x (-front~>X))
        xend = xstart + front~>X
        ystart = min (back~>Y) (max y (-front~>Y))
        yend = ystart + front~>Y
    create (\(x',y') ->
            if x' >= xstart && x' < xend &&
               y' >= ystart && y' < yend
            then front!(x'-xstart+1, y'-ystart+1)
            else back!(x',y'))
           (dim back)

-- Recorte entre dos puntos
crop :: Image a => Point2D -> Point2D -> Result a -> Result a
crop (x1,y1) (x2,y2) img = do
    m <- img
    let xl = max 0 (min x1 x2)
        xr = min (m~>X) (max x1 x2)
        yl = max 0 (min y1 y2)
        yr = min (m~>Y) (max y1 y2)
    create (\(x,y) -> m!(xl + x, yl + y)) (xr-xl, yr-yl)

-- Reflexiones
refH :: Image a => Result a -> Result a
refH img = do
    m <- img
    create (\(x,y) -> m!(x, m~>Y - y + 1)) (dim m)

refV :: Image a => Result a -> Result a
refV img = do
    m <- img
    create (\(x,y) -> m!(m~>X - x + 1, y)) (dim m)


-- Rotaciones (sentido horario)
rot90 :: Image a => Result a -> Result a
rot90 img = do
    m <- img
    create (\(x,y) -> m!(m~>X - y + 1, x)) (m~>Y, m~>X)

rot180 :: Image a => Result a -> Result a
rot180 img = do
    m <- img
    create (\(x,y) -> m!(m~>X - x + 1, m~>Y - y + 1)) (m~>X, m~>Y)

rot270 :: Image a => Result a -> Result a
rot270 img = do
    m <- img
    create (\(x,y) -> m!(y, m~>Y - x + 1)) (m~>Y, m~>X)


-- Une dos imágenes, una al lado de la otra
infixl 4 <|>
(<|>) :: Image a => Result a -> Result a -> Result a
img1 <|> img2 = do
    left  <- img1
    right <- img2
    if left~>Y /= right~>Y
        then throw "(<|>) : sizes do not match"
        else let f (x,y) = if x <= left~>X
                                then left !(x, y)
                                else right!(x - left~>X, y)
             in create f (left~>X + right~>X, left~>Y)

-- Une dos imágenes, una encima de la otra
infixl 3 </>
(</>) :: Image a => Result a -> Result a -> Result a
img1 </> img2 = do
    upper <- img1
    lower <- img2
    if upper~>X /= lower~>X
        then throw "(</>) : sizes do not match"
        else let f (x,y) = if y <= upper~>Y
                                then upper!(x, y)
                                else lower!(x, y - upper~>Y)
             in create f (upper~>X, upper~>Y + lower~>Y)
