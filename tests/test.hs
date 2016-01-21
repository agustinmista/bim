import BIM

main = do
    warhol <- load "img/warhol.bmp"
    toucan <- load "img/toucan.bmp"
    -- crayons <- load "img/crayons.bmp"
    -- noisy <- load "img/noisy.bmp"
    -- let colors = dominantColors 3 toucan
    save "img/out.bmp" (overlap (-50,-50) toucan warhol)
    -- save "img/out.bmp" (create (\(x,y) -> fst $ head colors) (100,100))
    -- save "img/out.bmp" (palette (500,100) 6 warhol)
