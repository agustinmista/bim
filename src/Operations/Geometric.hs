module Operations.Geometric (
    solid, crop, overlap,
    scale, scale2D, scaleTo,
    rot90, rot180, rot270,
    reflect, (<|>), (</>),
) where

import Image

-- Crea una imagen de color sólido de determinado tamaño
solid :: Image a => Pixel -> Point2D -> Result a
solid c s = create (\_ -> c) s

-- Recorte entre dos puntos
crop :: Image a => Point2D -> Point2D -> Result a -> Result a
crop (x1,y1) (x2,y2) img = do
    m <- img
    let xl = max 0 (min x1 x2)
        xr = min (m~>X) (max x1 x2)
        yl = max 0 (min y1 y2)
        yr = min (m~>Y) (max y1 y2)
    create (\(x,y) -> m!(xl + x, yl + y)) (xr-xl, yr-yl)


-- Superposición de una imagen sobre otra
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


-- Escalados, usando método del vecino más cercano
scale2D :: Image a => (Float, Float) -> Result a -> Result a
scale2D (xf,yf) img
    | xf < 0 || yf < 0 = throwError "scale: new size must be positive"
    | otherwise        = do
            m <- img
            create (\(x,y) -> m!(pos x xf, pos y yf)) (m~>X %> xf, m~>Y %> yf)
                        where pos v vf = ceiling $ fromIntegral v / vf

scaleTo :: Image a => Point2D -> Result a -> Result a
scaleTo (x',y') img = do
    m <- img
    let xf = fromIntegral x' / fromIntegral (m~>X)
        yf = fromIntegral y' / fromIntegral (m~>Y)
    scale2D (xf, yf) img

scale :: Image a => Float -> Result a -> Result a
scale f = scale2D (f,f)


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


-- Reflexiones
reflect :: Image a => Dim -> Result a -> Result a
reflect d img = do
    m <- img
    case d of
        X -> create (\(x,y) -> m!(x, m~>Y - y + 1)) (dim m)
        Y -> create (\(x,y) -> m!(m~>X - x + 1, y)) (dim m)


-- Union vertical y horizontal
infixl 4 <|>
infixl 3 </>

(<|>) :: Image a => Result a -> Result a -> Result a
img1 <|> img2 = do
    left  <- img1
    right <- img2
    if left~>Y /= right~>Y
        then throwError "(<|>) : sizes do not match"
        else let f (x,y) = if x <= left~>X
                                then left !(x, y)
                                else right!(x - left~>X, y)
             in create f (left~>X + right~>X, left~>Y)

(</>) :: Image a => Result a -> Result a -> Result a
img1 </> img2 = do
    upper <- img1
    lower <- img2
    if upper~>X /= lower~>X
        then throwError "(</>) : sizes do not match"
        else let f (x,y) = if y <= upper~>Y
                                then upper!(x, y)
                                else lower!(x, y - upper~>Y)
             in create f (upper~>X, upper~>Y + lower~>Y)
