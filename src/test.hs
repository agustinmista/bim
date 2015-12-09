import ImgProc

crayons = load "../img/crayons.bmp" :: Proc MatImg
lena    = load "../img/lena.bmp"    :: Proc MatImg
tools   = load "../img/tools.bmp"   :: Proc MatImg

out =  crayons*=0.3 <|> refV (crayons#>G)
                    </>
      -crayons      <|> lvl R ((+)100) crayons


test = runProc $ save out "../img/out.bmp"
