module Main where

import Data.List
import Data.Char

import Lib

splitWhen :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> x : splitWhen p xs''
                        where (x, xs'') = break p xs'


type Positions = [(Int, Int)]
newtype Tetrimino = Tetrimino { getPositions :: Positions }


readTetrimino :: [String] -> Maybe Tetrimino
readTetrimino ls
    | length ls /= 4                   = Nothing
    | any ((/= 4) . length) ls         = Nothing
    | any (any (flip notElem "#.")) ls = Nothing
    | otherwise = Just $ Tetrimino $
        map (\i -> (i `div` 4, i `mod` 4)) $ elemIndices '#' $ concat ls


readTetriminos :: String -> Maybe [Tetrimino]
readTetriminos s = let s' = trimSpaces s
                       ls = map trimSpaces (lines s')
                   in  sequence $ map readTetrimino $ splitWhen (=="") ls
    where trimSpaces = dropWhile isSpace . dropWhileEnd isSpace


instance Show Tetrimino where
    show (Tetrimino t) = let indices  = map (\(y, x) -> y * 4 + x) t
                             r0       = map (\i -> if i `elem` indices then '#' else '.') [0..16]
                             (l1, r1) = splitAt 4 r0
                             (l2, r2) = splitAt 4 r1
                             (l3, r3) = splitAt 4 r2
                             (l4, _)  = splitAt 4 r3
                         in  intercalate "\n" [l1, l2, l3, l4] ++ "\n"

shift :: Int -> Int -> Tetrimino -> Tetrimino
shift y x (Tetrimino t) = Tetrimino $ map (\(y', x') -> (y' + y, x' + x)) t

normalizeY :: Tetrimino -> Tetrimino
normalizeY t
    | all ((/= 0) . fst) (getPositions t) = normalizeY $ shift (-1) 0 t
    | otherwise = t

normalizeX :: Tetrimino -> Tetrimino
normalizeX t
    | all ((/= 0) . snd) (getPositions t) = normalizeX $ shift 0 (-1) t
    | otherwise = t

normalize :: Tetrimino -> Tetrimino
normalize = normalizeY . normalizeX

overlap :: [Tetrimino] -> Tetrimino -> Bool
overlap ts (Tetrimino tPos) = any ((/= 0) . length . (intersect tPos)) tsPos
    where tsPos = map getPositions ts

solve :: [Tetrimino] -> (Int, [Tetrimino])
solve ts = solveRec 2 [] ts
    where solveRec :: Int -> [Tetrimino] -> [Tetrimino] -> (Int, [Tetrimino])
          solveRec size state [] = (size, state)
          solveRec size state (t:ts) =
            if overlap state t then next
                               else solveRec size (t:state) ts
                where rightTetrimino = shift 0 1 t
                      downTetrimino = shift 1 0 $ normalizeX t
                      inBound (Tetrimino t) = all (\(y, x) -> y < size && x < size) t
                      next
                        | inBound rightTetrimino = solveRec size state (rightTetrimino:ts)
                        | inBound downTetrimino  = solveRec size state (downTetrimino:ts)
                        | otherwise              = solveRec (size + 1) state (t:ts)

showSolve :: (Int, [Tetrimino]) -> String
showSolve (size, ts) =

main :: IO ()
main = do
    content <- getContents
    case readTetriminos content of
        Nothing -> print "Invalid tetriminos format "
        Just ts -> let ts' = map normalize ts
                   in  mapM_ print $ snd $ solve ts



-- main = print $ readTetriminos $ intercalate "\n" [ "###."
--                                                  , ".#.."
--                                                  , "...."
--                                                  , "...."
--                                                  , ""
--                                                  , "...."
--                                                  , "####"
--                                                  , "...."
--                                                  , "...."
--                                                  ]
