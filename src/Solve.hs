module Solve
    ( solve
    , showSolve
    ) where


import Data.List (find)
import Data.Maybe (isJust, fromJust)
import Control.Monad (join)
import Control.Applicative ((<|>))

import Debug.Trace

import Tetrimino


solve :: [Tetrimino] -> [Tetrimino]
solve ts = fromJust $ join $ find isJust $ map (solveSize ts) [minSize..]
    where minSize = ceiling $ logBase 2 $ fromIntegral cellCount
          cellCount = sum $ map (length . getPositions) ts


solveSize :: [Tetrimino] -> Int -> Maybe [Tetrimino]
solveSize ts size = solveRec size [] ts



-- func backtrack(state)
--      if accept(state)
--          return Just state
--      if reject(state)
--          return Nothing
--      for subState in availableStates(state)
--          backtrack(subState)
--      end
-- end




solveRec :: Int -> [Tetrimino] -> [Tetrimino] -> Maybe [Tetrimino]
solveRec size state ts
    | length state == length ts = Just state
    | otherwise = do
        subStates <- sequence
                     $ filter isJust
                     $ map (firstValidSpot size state)
                     $ filter (flip notElem state) ts
        join $ find isJust $ map (\s -> solveRec size s ts) subStates


firstValidSpot :: Int -> [Tetrimino] -> Tetrimino -> Maybe [Tetrimino]
firstValidSpot size state t@(Tetrimino pos)
    | not $ overlap state t          = Just (t:state)
    | any (\(y, _) -> y >= size) pos = Nothing
    | any (\(_, x) -> x >= size) pos = firstValidSpot size state downTetrimino
    | otherwise                      = firstValidSpot size state rightTetrimino
    where rightTetrimino = shift 0 1 t
          downTetrimino  = shift 1 0 $ normalizeX t


showSolve :: [Tetrimino] -> String
showSolve [] = "Empty solve"
showSolve ts = foldl1 (zipWith pickIDChar) $ zipWith (showWithSizeAndId size) chars ts
    where chars = ['A'..'Z'] ++ ['a'..'z'] ++ repeat '#'
          pickIDChar '.' c = c
          pickIDChar c   _ = c
          size = maximum $ map (\x -> maximum (map (uncurry max) (getPositions x))) ts
