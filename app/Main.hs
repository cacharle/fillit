module Main where

import Tetrimino
import Solve


main :: IO ()
main = do
    content <- getContents
    case readTetriminos content of
        Left err -> putStrLn $ "Error: Invalid tetriminos format: " ++ err
        -- Right ts -> mapM_ print ts
        Right ts -> putStr $ showSolve $ solve ts
