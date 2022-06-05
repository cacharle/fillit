module Solve
    ( --solve
    ) where


import Data.List (find)
import Data.Maybe (isJust, fromJust)
import Control.Monad (join)
import Control.Applicative ((<|>))

import Debug.Trace

import Tetrimino


-- solve :: [Tetrimino] -> [Tetrimino]
-- solve ts = fromJust $ join $ find isJust $ map (\s -> trace ("solve with size: " ++ (show s)) (solveSize ts s)) [minSize..]
--     where minSize = ceiling $ logBase 2 $ fromIntegral cellCount
--           cellCount = sum $ map (length . getPositions) ts
--
-- solveSize :: [Tetrimino] -> Int -> Maybe [Tetrimino]
-- solveSize ts size = solveRec size [] ts
--
-- solveRec :: Int -> [Tetrimino] -> [Tetrimino] -> Maybe [Tetrimino]
-- solveRec _ state [] = Just state
-- solveRec size state ts@(t:rest) = join $ find isJust $ map solveRec size rest $ map next ts
--
-- solveRec size [] (t:ts) = trace "hello" $ solveRec' size [t] ts
-- -- solveRec size [] (t:ts) = trace "WAW" $
-- --     trace "lets solve one" (solveOne' size [] t)
-- --     <|> trace "lets backtrace " (solveRec' size [] ((normalize t):(normalize stateHead):ts))
-- solveRec size state@(stateHead:stateRest) (t:ts) = trace "WAW" $
--     (solveOne size state t) >>= (\s -> solveRec' size s ts)
--     <|> solveOne size stateRest t' (stateHead':ts)
--     where t'         = normalize t
--           stateHead' = normalize stateHead
--
--
--     -- solveRec' size stateRest ((normalize t):ts ++ [normalize stateHead])
--
-- solveOne :: Int -> [Tetrimino] -> Tetrimino -> Maybe [Tetrimino]
-- solveOne size state t@(Tetrimino pos)
--     | not $ overlap state t          = Just (t:state)
--     | any (\(y, _) -> y >= size) pos = Nothing
--     | any (\(_, x) -> x >= size) pos = solveOne size state downTetrimino
--     | otherwise                      = solveOne size state rightTetrimino
--     where rightTetrimino = shift 0 1 t
--           downTetrimino  = shift 1 0 $ normalizeX t
--
-- solveRec' x state y    = trace (showSolve state) (solveRec x state y)
-- solveOne' size state t = trace (showSolve state) (solveOne size state t)
--
-- showSolve :: [Tetrimino] -> String
-- showSolve [] = "Empty solve"
-- showSolve ts = foldl1 (zipWith pickIDChar) $ zipWith (showTetrimino size) chars ts
--     where chars = ['A'..'Z'] ++ ['a'..'z'] ++ repeat '#'
--           pickIDChar '.' c = c
--           pickIDChar c   _ = c
--           size = maximum $ map (\x -> maximum (map (uncurry max) (getPositions x))) ts
