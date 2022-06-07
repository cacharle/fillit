module Tetrimino
    ( Tetrimino(..)
    , Positions(..)
    , fromPositions
    , toPositions
    , scale
    , readTetriminoFromLines
    , readTetriminos
    , shift
    , normalize
    , normalizeY
    , normalizeX
    , overlap
    , showWithSizeAndId
    ) where


import Data.Bits (shiftR, shiftL, (.|.), (.&.)) -- TODO: try unsafeShiftL
import Data.List (intersect, elemIndices, dropWhileEnd)
import Data.List.Split (splitOn)
import Data.Char (isSpace)

import Debug.Trace

newtype Positions = Positions { getPositions :: [(Int, Int)] }
data Tetrimino = Tetrimino { getSize :: Int, getBits :: Int }


positionsSize :: Positions -> Int
positionsSize (Positions xs) = 1 + (maximum $ map (uncurry max) xs)


toPositions :: Tetrimino -> Positions
toPositions (Tetrimino size b) = Positions
                                     [(y, x)
                                     | y <- indices
                                     , x <- indices
                                     , ((positionToBits size (y, x)) .&. b) /= 0
                                     ]
    where indices = [0..(size - 1)]


fromPositionsSize :: Int -> Positions -> Tetrimino
fromPositionsSize size pos@(Positions xs) =
    Tetrimino size $ foldl1 (.|.) $ map (positionToBits size) xs

fromPositions :: Positions -> Tetrimino
fromPositions pos = fromPositionsSize (positionsSize pos) pos


positionToBits :: Int -> (Int, Int) -> Int
positionToBits size (y, x) = (1 `shiftL` x) `shiftL` (size * y)


scale :: Int -> Tetrimino -> Tetrimino
scale size t = fromPositionsSize (size + 1) (toPositions t)


shift :: Int -> Int -> Tetrimino -> Tetrimino
shift 0 x (Tetrimino size b)
    | x > 0 = Tetrimino size (b `shiftL` x)
    | x < 0 = Tetrimino size (b `shiftR` (-x))
shift y 0 (Tetrimino size b)
    | y > 0 = Tetrimino size (b `shiftL` (y * size))
    | y < 0 = Tetrimino size (b `shiftR` ((-y) * size))
shift y x t = shift y 0 $ shift 0 x t


normalizeX :: Tetrimino -> Tetrimino
normalizeX t@(Tetrimino size b)
    | leftBarMask .&. b == 0 = normalizeX $ shift 0 (-1) t
    | otherwise = t
    where leftBarMask = foldl1 (.|.) [positionToBits size (y, 0) | y <- [0..(size - 1)]]


normalizeY :: Tetrimino -> Tetrimino
normalizeY t@(Tetrimino size b)
    | topBarMask .&. b == 0 = normalizeY $ shift (-1) 0 t
    | otherwise = t
    where topBarMask = foldl1 (.|.) [positionToBits size (0, x) | x <- [0..(size - 1)]]

normalize :: Tetrimino -> Tetrimino
normalize = normalizeY . normalizeX


overlap :: [Tetrimino] -> Tetrimino -> Bool
overlap ts (Tetrimino size b) = mask .&. b /= 0
    where mask = foldl1 (.|.) $ map getBits ts


instance Eq Tetrimino where
    t1 == t2 = let (Tetrimino _ b1) = normalize t1
                   (Tetrimino _ b2) = normalize t2
               in b1 == b2


readTetriminoFromLines :: [String] -> Either String Tetrimino
readTetriminoFromLines ls
    | length ls /= 4                   = Left "Has to be 4 lines long"
    | any ((/= 4) . length) ls         = Left "Each line has to be 4 characters long"
    | any (any (flip notElem "#.")) ls = Left "Can only contain '#' and '.'"
    | otherwise = if isValid parsedPos
                    then Right $ normalize $ fromPositions parsedPos
                    else Left "Has to contain 4 cell attached to each other"
    where parsedPos = Positions $
            map (\i -> (i `div` 4, i `mod` 4)) $ elemIndices '#' $ concat ls

readTetrimino :: String -> Either String Tetrimino
readTetrimino = readTetriminoFromLines . lines

readTetriminos :: String -> Either String [Tetrimino]
readTetriminos s = let s' = trimSpaces s
                       ls = map trimSpaces (lines s')
                   in  sequence $ map readTetriminoFromLines $ splitOn [""] ls
    where trimSpaces = dropWhile isSpace . dropWhileEnd isSpace

isValid :: Positions -> Bool
isValid (Positions pos) = length pos == 4 && all isConnectedPos pos
    where isConnectedPos (y, x) = any (flip elem shifts) pos
                where shifts = [ (y + 1, x)
                               , (y - 1, x)
                               , (y, x + 1)
                               , (y, x - 1)
                               ]


showWithSizeAndId :: Int -> Char -> Positions -> String
showWithSizeAndId squareSize c (Positions pos) =
    unlines [
        [if (y, x) `elem` pos then c else '.' | x <- [0..(squareSize - 1)]]
        | y <- [0..(squareSize - 1)]
    ]

instance Show Tetrimino where
    show = show . toPositions

instance Show Positions where
    show pos = showWithSizeAndId (positionsSize pos) '#' pos
