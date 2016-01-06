import ImgProc

test = do
    tools <- load "../img/tools.bmp"
    lena <- load "../img/lena.bmp"
    let neg = pixelTrans (\pos pix -> -pix) tools
        out = tools#>G + tools#>B
    save "../img/out.bmp" out
