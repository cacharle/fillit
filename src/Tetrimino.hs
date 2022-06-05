module Tetrimino
    ( Tetrimino(..)
    , readTetrimino
    , readTetriminos
    , shift
    , normalize
    , normalizeY
    , normalizeX
    , overlap
    , showTetrimino
    ) where


import Data.List
import Data.Char (isSpace)


type Positions = [(Int, Int)]
newtype Tetrimino = Tetrimino { getPositions :: Positions }


readTetrimino :: [String] -> Either String Tetrimino
readTetrimino ls
    | length ls /= 4                   = Left "Has to be 4 lines long"
    | any ((/= 4) . length) ls         = Left "Each line has to be 4 characters long"
    | any (any (flip notElem "#.")) ls = Left "Can only contain '#' and '.'"
    | otherwise = Right $ Tetrimino $
        map (\i -> (i `div` 4, i `mod` 4)) $ elemIndices '#' $ concat ls


readTetriminos :: String -> Either String [Tetrimino]
readTetriminos s = let s' = trimSpaces s
                       ls = map trimSpaces (lines s')
                   in  sequence $ map readTetrimino $ splitWhen (=="") ls
    where trimSpaces = dropWhile isSpace . dropWhileEnd isSpace
          -- TODO: install split package
          splitWhen :: Eq a => (a -> Bool) -> [a] -> [[a]]
          splitWhen p xs = case dropWhile p xs of
                              []  -> []
                              xs' -> x : splitWhen p xs''
                                  where (x, xs'') = break p xs'


showTetrimino :: Int -> Char -> Tetrimino -> String
showTetrimino squareSize c (Tetrimino pos) =
    unlines [
        [if (y, x) `elem` pos then c else '.' | x <- [0..squareSize]]
        | y <- [0..squareSize]
    ]

instance Show Tetrimino where
    show t@(Tetrimino pos) = showTetrimino squareSize '#' t
        where squareSize = maximum (map (uncurry max) pos)


isValid :: Tetrimino -> Bool
isValid (Tetrimino pos) = length pos == 4 && all isValidPos pos
    where isValidPos (y, x) = all (flip notElem shifts) pos
                where shifts = [ (y + 1, x)
                        , (y - 1, x)
                        , (y, x + 1)
                        , (y, x - 1)
                        ]


shift :: Int -> Int -> Tetrimino -> Tetrimino
shift y x (Tetrimino t) = Tetrimino $ map (\(y', x') -> (y' + y, x' + x)) t

normalizeY :: Tetrimino -> Tetrimino
normalizeY t@(Tetrimino pos)
    | all ((/= 0) . fst) pos = normalizeY $ shift (-1) 0 t
    | otherwise = t

normalizeX :: Tetrimino -> Tetrimino
normalizeX t@(Tetrimino pos)
    | all ((/= 0) . snd) pos = normalizeX $ shift 0 (-1) t
    | otherwise = t

normalize :: Tetrimino -> Tetrimino
normalize = normalizeY . normalizeX

overlap :: [Tetrimino] -> Tetrimino -> Bool
overlap ts (Tetrimino tPos) = any ((/= 0) . length . (intersect tPos)) tsPos
    where tsPos = map getPositions ts
