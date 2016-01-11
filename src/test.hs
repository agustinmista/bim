import ImgProc

test = do
    tools <- load "../img/tools.bmp"
    lena <- load "../img/lena.bmp"
    hist tools
