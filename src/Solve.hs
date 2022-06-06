module Solve
    ( solve
    , solveSize
    , showSolve
    ) where


import Data.List (find, delete)
import Data.Maybe (isJust, fromJust)
import Control.Monad (join)
import Control.Applicative ((<|>))

import Debug.Trace

import Tetrimino


solve :: [Tetrimino] -> [Tetrimino]
solve ts = fromJust $ join $ find isJust $ map (solveSize ts) [(minSize - 1)..]
    where minSize = ceiling $ logBase 2 $ fromIntegral cellCount
          cellCount = sum $ map (length . getPositions) ts


solveSize :: [Tetrimino] -> Int -> Maybe [Tetrimino]
solveSize ts size = solveRec [] ts
    where

        solveRec :: [Tetrimino] -> [Tetrimino] -> Maybe [Tetrimino]
        solveRec state [] = Just state
        solveRec state ts = do
            subStates <- sequence $ filter isJust $ map (firstValidSpot . normalize) $ ts
            join $ find isJust
                $ map (\subState@(s:_) -> solveRec subState (delete s ts)) subStates
            where

                firstValidSpot :: Tetrimino -> Maybe [Tetrimino]
                firstValidSpot t@(Tetrimino pos)
                    | not $ overlap state t  = Just (t:state)
                    | inBound rightTetrimino = firstValidSpot rightTetrimino
                    | inBound downTetrimino  = firstValidSpot downTetrimino
                    | otherwise              = Nothing
                    where rightTetrimino = shift 0 1 t
                          downTetrimino  = shift 1 0 $ normalizeX t
                          inBound (Tetrimino pos) = all (\(y, x) -> y < size && x < size) pos


showSolve :: [Tetrimino] -> String
showSolve [] = "Empty solve"
showSolve ts = concat
               $ foldl1 (zipWith pickIDChar)
               $ map (map (:[]))
               $ zipWith (showWithSizeAndId size) chars ts
    where size = maximum $ map (\x -> maximum (map (uncurry max) (getPositions x))) ts
          chars = ['A'..'Z'] ++ ['a'..'z'] ++ repeat '#'
          ansiColorCodes = [31..37] ++ [90..97]
          colorId id = case find ((== head id) . fst) (zip chars ansiColorCodes) of
                        Just (_, code) -> "\o33[" ++ show code ++ "m" ++ id ++ "\o33[0m"
                        Nothing        -> id
          pickIDChar "." id = colorId id
          pickIDChar id  _  = colorId id
