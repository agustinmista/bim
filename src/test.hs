import ImgProc

crayons = load "../img/crayons.bmp" :: Proc MatImg
lena    = load "../img/lena.bmp"    :: Proc MatImg
tools   = load "../img/tools.bmp"   :: Proc MatImg

out = refH tools <|> refV (tools#>G)


test = runProc $ save (bright ((+) 100) tools) "../img/test.bmp"
