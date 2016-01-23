import BIM

-- modify
testModify = do
    toucan <- load "img/toucan.bmp"
    let modified = modify (+50) toucan
    save "tests/outputs/modify.bmp" (toucan <|> modified)

-- modifyCh
testModifyCh = do
    toucan <- load "img/toucan.bmp"
    let modifiedR = modifyCh R (*2)     toucan
        modifiedG = modifyCh G ((-)100) toucan
        modifiedB = modifyCh B (+50)    toucan
        out = toucan    <|> modifiedR
                        </>
              modifiedG <|> modifiedB
    save "tests/outputs/modifyCh.bmp" out

-- (#>)
testGetChannel = do
    toucan <- load "img/toucan.bmp"
    let out = toucan <|> toucan#>R <|> toucan#>G <|> toucan#>B
    save "tests/outputs/getChannel.bmp" out

-- negative
testNegative = do
    crayons <- load "img/crayons.bmp"
    let neg = negative crayons
    save "tests/outputs/negative.bmp" (crayons <|> neg)

-- mono
testMonochrome = do
    crayons <- load "img/crayons.bmp"
    let monoLum = mono lum crayons
        monoLig = mono lig crayons
        monoAvg = mono avg crayons
        out = crayons <|> monoLum
                      </>
              monoLig <|> monoAvg
    save "tests/outputs/monochrome.bmp" out

-- threshold y binary
testThreshold = do
    tools <- load "img/tools.bmp"
    let out = tools
         </>  threshold lum 0.5 tools
         </>  binary lum 0.5 tools
    save "tests/outputs/threshold.bmp" out
