module Tetrimino
    ( Tetrimino(..)
    , Positions(..)
    , Direction(..)
    , hitsBorder
    , hitsBorderMasks
    , hitsBorderMask
    , fromPositions
    , toPositions
    , scale
    , readTetrimino
    , readTetriminos
    , shift
    , normalize
    , normalizeY
    , normalizeX
    , normalizeXMasks
    , overlap
    , showWithSizeAndId
    , masksFromSize
    ) where


import           Data.Bits       (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import           Data.Char       (isSpace)
import           Data.List       (dropWhileEnd, elemIndices, intersect)
import           Data.List.Split (splitOn)

import           Debug.Trace

import           Data.Char
import           Numeric

newtype Positions = Positions { getPositions :: [(Int, Int)] }
data Tetrimino = Tetrimino { getSize :: Int, getBits :: Int }


positionsSize :: Positions -> Int
positionsSize (Positions xs) = 1 + (maximum $ map (uncurry max) xs)


toPositions :: Tetrimino -> Positions
toPositions (Tetrimino size b) = Positions
                                     [ (y, x)
                                     | y <- indices
                                     , x <- indices
                                     , positionToBits size (y, x) .&. b /= 0
                                     ]
    where indices = [0..(size - 1)]


fromPositionsSize :: Int -> Positions -> Tetrimino
fromPositionsSize size (Positions xs) =
    Tetrimino size $ foldl1 (.|.) $ map (positionToBits size) xs

fromPositions :: Positions -> Tetrimino
fromPositions pos = fromPositionsSize (positionsSize pos) pos


positionToBits :: Int -> (Int, Int) -> Int
positionToBits size (y, x) = (1 `unsafeShiftL` x) `unsafeShiftL` (size * y)


scale :: Int -> Tetrimino -> Tetrimino
scale size t = fromPositionsSize size (toPositions t)


shift :: Int -> Int -> Tetrimino -> Tetrimino
shift 0 x (Tetrimino size b)
    | x >= 0 = Tetrimino size (b `unsafeShiftL` x)
    | x < 0 = Tetrimino size (b `unsafeShiftR` (-x))
shift y 0 (Tetrimino size b)
    | y >= 0 = Tetrimino size (b `unsafeShiftL` (y * size))
    | y < 0 = Tetrimino size (b `unsafeShiftR` ((-y) * size))
shift y x t = shift y 0 $ shift 0 x t


data Direction = DUp | DDown | DLeft | DRight deriving Show


showBits :: Int -> String
showBits b = showIntAtBase 2 intToDigit b ""

masksFromSize :: Int -> (Int, Int, Int, Int)
masksFromSize size = ( (1 `unsafeShiftL` size) - 1
                     , ((1 `unsafeShiftL` size) - 1) `unsafeShiftL` (size * (size - 1))
                     , foldl1 (.|.) [1 `unsafeShiftL` (size * j) | j <- [0..(size - 1)]]
                     , foldl1 (.|.) [(1 `unsafeShiftL` (size - 1)) `unsafeShiftL` (size * j) | j <- [0..(size - 1)]]
                     )

hitsBorderMask :: Int -> Tetrimino -> Bool
hitsBorderMask mask (Tetrimino _ b) = mask .&. b /= 0

hitsBorderMasks :: Direction -> (Int, Int, Int, Int) -> Tetrimino -> Bool
hitsBorderMasks dir (up, down, left, right) t@(Tetrimino size b) = mask .&. b /= 0
    where
        mask = case dir of
            DUp    -> up
            DDown  -> down
            DLeft  -> left
            DRight -> right


hitsBorder :: Direction -> Tetrimino -> Bool
hitsBorder dir t@(Tetrimino size _) = hitsBorderMasks dir (masksFromSize size) t


normalizeXMasks :: Int -> Tetrimino -> Tetrimino
normalizeXMasks m t@(Tetrimino size b) =
    if hitsBorderMask m t
        then t
        else normalizeXMasks m $ shift 0 (-1) t

normalizeX :: Tetrimino -> Tetrimino
normalizeX t@(Tetrimino size _) = normalizeXMasks leftMask t
    where (_, _, leftMask, _) = masksFromSize size

normalizeY :: Tetrimino -> Tetrimino
normalizeY t@(Tetrimino size b) =
    if hitsBorder DUp t
        then t
        else normalizeY $ shift (-1) 0 t


normalize :: Tetrimino -> Tetrimino
normalize = normalizeY . normalizeX


overlap :: Int -> Tetrimino -> Bool
overlap mask (Tetrimino _ b) = mask .&. b /= 0


instance Eq Tetrimino where
    t1 == t2 = (getSize t1) == (getSize t2)
               && (getBits (normalize t1)) == (getBits (normalize t2))


readTetriminoFromLines :: [String] -> Either String Tetrimino
readTetriminoFromLines ls
    | length ls /= 4                   = Left "Has to be 4 lines long"
    | any ((/= 4) . length) ls         = Left "Each line has to be 4 characters long"
    | any (any (flip notElem "#.")) ls = Left "Can only contain '#' and '.'"
    | otherwise = if isValid parsedPos
                    then Right $ fromPositions parsedPos
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


showPositionsWithSizeAndId :: Int -> Char -> Positions -> String
showPositionsWithSizeAndId squareSize c (Positions pos) =
    unlines [
        [if (y, x) `elem` pos then c else '.' | x <- [0..(squareSize - 1)]]
        | y <- [0..(squareSize - 1)]
    ]


showWithSizeAndId :: Int -> Char -> Tetrimino -> String
showWithSizeAndId size c t = showPositionsWithSizeAndId size c (toPositions t)


instance Show Tetrimino where
    show t@(Tetrimino size _) = let pos = toPositions t
                                in  showPositionsWithSizeAndId size '#' pos


instance Show Positions where
    show (Positions []) = "Empty Positions"
    show pos            = showPositionsWithSizeAndId (positionsSize pos) '#' pos
