module Main where

import Tetrimino
import Solve


-- main :: IO ()
-- main = do
--     content <- getContents
--     case readTetriminos content of
--         Left err -> putStrLn $ "Error: Invalid tetriminos format: " ++ err
--         Right ts -> if length ts < 1 || length ts > 26
--                     then putStrLn $ "Error: The number of tetriminos in the file must be between 1 and 26"
--                     else putStr $ showSolve $ solve ts


main :: IO ()
main = print $ normalize $ shift 3 3 $ scale 10 $ fromPositions $ Positions [(1, 2), (2, 2)]
-- main = print $ toPositions $ Tetrimino 4 7
