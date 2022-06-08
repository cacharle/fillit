module Main where

import           Solve
import           Tetrimino


main :: IO ()
main = do
    content <- getContents
    case readTetriminos content of
        Left err -> putStrLn $ "Error: Invalid tetriminos format: " ++ err
        Right ts -> if length ts < 1 || length ts > 26
                    then putStrLn $ "Error: The number of tetriminos in the file must be between 1 and 26"
                    else putStr $ showSolve $ solve ts


-- main :: IO ()
-- main = let t = shift 0 (-2) $ scale 7 $ fromPositions $ Positions [(1, 1), (1, 2), (2, 1), (2, 2)]
--        in do
--         print t
--         -- print (normalize t)
--         -- print (normalizeX t)
--         -- print (normalizeY t)
--         print $ hitsBorder DLeft t
