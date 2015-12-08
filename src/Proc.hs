module Proc (
    module Proc,
)where

import Control.Monad.Trans.Except

-- Representamos un procesamiento como una computación que
-- puede fallar, devolviendo un String, o bien tener éxito, efectuando
-- algunas operaciones de entrada/salida.
type Proc a = ExceptT String IO a

-- Corremos un procesamiento
runProc :: Proc a -> IO ()
runProc p = do res <- runExceptT p
               case res of
                   Left err -> putStrLn $ "Error: " ++ err
                   Right _  -> putStrLn "Success!"

-- Renombramos el lanzamiento de excepciones
failProc :: String -> Proc a
failProc = throwE
