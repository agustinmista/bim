import BIM

testArithmetics = do
    rubik <- load "img/rubik.bmp"
    mask  <- load "img/mask.bmp"
    let out = (rubik + mask) <|> (rubik - mask)
                             </>
              (rubik * mask) <|> (blend 0.5 mask rubik)
    save "tests/outputs/arithmetics.bmp" out
