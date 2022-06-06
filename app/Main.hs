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

        -- Right ts -> case solveSize ts 6 of
        --                 Just ts -> do putStr $ showSolve ts
        --                               print $ map getPositions ts
        --                 Nothing -> putStrLn "noooo"
