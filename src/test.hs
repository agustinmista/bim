import Prelude       hiding ((!!), catch)
import Data.Matrix   hiding ((!), (<|>))
import Control.Exception (catch, IOException)
import Data.Tuple        (swap)
import Codec.BMP
import Data.Word
import qualified Data.Foldable   as F
import qualified Data.ByteString as BS

testParse = do
    readed <- readBMP "img/out.bmp"
    case readed of
        Right bmp -> do
             let (x,y) = bmpDimensions bmp
             --putStrLn $ show $  toMatrix x y bmp
             putStrLn $ show $ fromLists $ reverse $ getRows x $ toTuples $ BS.foldr (:) [] $ unpackBMPToRGBA32 bmp
             --putStrLn $ show $  BS.foldl (\ts t -> ts ++ [t]) [] $ unpackBMPToRGBA32 bmp

toMatrix x y = fromList y x . toTuples . BS.foldl (flip (:)) [] . unpackBMPToRGBA32

getRows n [] = []
getRows n xs = let (row, rows) = splitAt n xs
                  in row : getRows n rows

toTuples [] = []
toTuples (r:g:b:_:xs) = (fromIntegral r, fromIntegral g, fromIntegral b) : toTuples xs
