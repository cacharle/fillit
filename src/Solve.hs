module Solve
    ( solve
    , solveSize
    , showSolve
    ) where


import Data.Bits ((.|.))
import Data.List (find, delete)
import Data.Maybe (isJust, fromJust)
import Control.Monad (join)
import Control.Applicative ((<|>))

import Tetrimino

import Debug.Trace


solve :: [Tetrimino] -> [Tetrimino]
solve ts = fromJust
           $ join
           $ find isJust
           $ map (solveSize (map normalize ts)) [minSize..]
    where minSize = ceiling $ logBase 2 $ fromIntegral cellCount
          cellCount = sum $ map (length . getPositions . toPositions) ts


solveSize :: [Tetrimino] -> Int -> Maybe [Tetrimino]
solveSize ts size = solveRec [] $ map (scale size) ts
    where

        solveRec :: [Tetrimino] -> [Tetrimino] -> Maybe [Tetrimino]
        solveRec state [] = Just state
        solveRec state ts = do
            subStates <- sequence $ filter isJust $ map (firstValidSpot ) $ ts
            join $ find isJust
                $ map (\subState@(s:_) -> solveRec subState (delete s ts)) subStates
            where

                stateMask = foldl (.|.) 0 $ map getBits state

                firstValidSpot :: Tetrimino -> Maybe [Tetrimino]
                firstValidSpot t
                    | not $ overlap stateMask t     = Just (t:state)
                    | not $ hitsBorder DRight t = firstValidSpot rightTetrimino
                    | not $ hitsBorder DDown t  = firstValidSpot downTetrimino
                    | otherwise                 = Nothing
                    where rightTetrimino = shift 0 1 t
                          downTetrimino  = shift 1 0 $ normalizeX t


showSolve :: [Tetrimino] -> String
showSolve [] = "Empty solve"
showSolve ts = concat
               $ foldl1 (zipWith pickIDChar)
               $ map (map (:[]))
               $ zipWith (showWithSizeAndId size) chars ts
    where size = maximum $ map getSize ts
          chars = ['A'..'Z'] ++ ['a'..'z'] ++ repeat '#'
          ansiColorCodes = [31..37] ++ [90..97]
          colorId id = case find ((== head id) . fst) (zip chars ansiColorCodes) of
                        Just (_, code) -> "\o33[" ++ show code ++ "m" ++ id ++ "\o33[0m"
                        Nothing        -> id
          pickIDChar "." id = colorId id
          pickIDChar id  _  = colorId id
