import BIM

testPalette = do
    warhol <- load "img/warhol.bmp"
    lena   <- load "img/lena.bmp"
    let wp = palette (600,100) 6 warhol
        lp = palette (600,100) 6 lena
    save "tests/outputs/paletteWarhol.bmp" wp
    save "tests/outputs/paletteLena.bmp" lp
