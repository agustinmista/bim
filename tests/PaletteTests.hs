import BIM

testPalette = do
    warhol <- load "img/warhol.bmp"
    lena   <- load "img/lena.bmp"
    let wp = warhol </> underPalette warhol
        lp = lena </> underPalette lena
    save "tests/outputs/paletteWarhol.bmp" wp
    save "tests/outputs/paletteLena.bmp" lp
            where underPalette img = do
                    m <- img
                    palette (m~>X, m~>X %> (1/6)) 6 img
