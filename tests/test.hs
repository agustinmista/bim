import BIM

test = do
    toucan <- load "img/toucan.bmp"
    crayons <- load "img/crayons.bmp"
    -- save "img/out.bmp" (create (\(x,y) -> (x*10,y*10,0)) (10,5))
    save "img/out.bmp" (blur crayons)
