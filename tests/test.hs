import BIM

test = do
    tools <- load "img/crayons.bmp"
    let out = tools#>R <|> tools#>G
          </> tools#>B <|> tools
    -- save "img/out.bmp" (create (\(x,y) -> (x*10,y*10,0)) (10,5))
    save "img/out.bmp" (extend  20 tools)
