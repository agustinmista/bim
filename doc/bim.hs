--------------------------------------------------------------------------------
--          eDSL básico para manipulación de mapas de bits                    --
--------------------------------------------------------------------------------

-- Represento un procesador de imágenes que puede fallar:
newtype Proc a = Proc { runProc :: Either String a }

-- Represento imágenes RGB como matrices de pixeles:
type Pixel = (Word8, Word8, Word8)
type Image = Matrix Pixel

{-  Luego, defino un conjunto de operaciones primitivas:

I.   Operaciones para importar/exportar:
          desde archivo: String -> IO (ImgProc Image)
          hacia archivo: Proc Image -> String -> IO ()
          de síntesis:   (Int -> Int -> Pixel) -> (Int, Int) -> Proc Image

II.  Operaciones sobre una imagen:
          pixel   a pixel: (Pixel -> Pixel) -> Image -> Proc Image
          vecinos a pixel: (Matrix Pixel -> Pixel) -> Image -> Proc Image
          global  a pixel: (Image -> Pixel) -> Image -> Proc Image

III. Operaciones sobre dos imágenes:
                        (Pixel -> Pixel -> Pixel) -> Image -> Image -> Proc Image

IV. Operaciones de propiedades:
            size :: Image -> (Int, Int)
            (!)  :: Image -> (Int, Int) -> Pixel
            (!>) :: Image -> Dim -> Int             (Dim:= H | W)

Y a partir de las anteriores, un conjunto de operaciones
derivadas orientadas al procesamiento de imágenes:

1. Operaciones aritméticas sobre imágenes (+, -, *, /, blend, &&, ||)
2. Operaciones puntuales (threshold, negative, levels, etc)
3. Operaciones sobre vecinos (Filtos) (blur, edge-detection, noise-reduction, etc)
4. Operaciones geometricas (rotate, scale, reflect, etc)
5. Operaciones de síntesis (collage, blank-image, noise-generation, rainbow, etc)
6. Operaciones de análisis (histogram, color-palette, etc)
7. Proyecciones y conversiones (channels, rgb <=> b&w, rgb <=> mask, etc)
8. ...

-}

-- Pequeños ejemplos de uso:
testAnalysis = do
    img <- load "test.bmp"  -- Cargo una imagen
    hst <- histogram img    -- Obtengo un histograma de la imagen
    chR <- R << img
    chG <- G << img         -- Obtengo los canales RGB de la imagen
    chB <- B << img
    res <- hst <|> chR      -- Formo una cuadrícula 2x2 con el histograma
               </>          -- y los canales de la imagen
           chG <|> chB
    save res "out.bmp"      -- Guardo el resultado en un archivo


testGrid = do                        -- Agrega una cuadrícula verde de 10x10
    img  <- load "myImg.bmp"
    create
        (\x y ->
            if x % (img!>H `div` 10) == 0 ||
               y % (img!>W `div` 10) == 0
            then green
            else img!(x,y))
        (size img)

-- Fuente de consulta:
-- http://homepages.inf.ed.ac.uk/rbf/HIPR2/wksheets.htm
