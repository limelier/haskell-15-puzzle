module Lib
    ( shuffleOnce
    ) where

import System.Random
import System.IO
import Data.List

type Puzzle = [[Int]]
puz :: Integral a => [[a]] -> Puzzle
puz [] = []
puz (hd:tl) = (puzRow hd) : (puz tl)
puzRow :: Integral a => [a] -> [Int]
puzRow [] = []
puzRow (hd:tl) = (fromIntegral hd) : (puzRow tl)

initPuz = puz [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 0]]

data Move = MUp | MDown | MLeft| MRight deriving (Enum, Show);
type Pos = (Int, Int)

invertMove :: Move -> Move
invertMove MUp = MDown
invertMove MDown = MUp
invertMove MLeft = MRight
invertMove MRight = MLeft

validPos :: Pos -> Bool -- allow only 0-3 for both pos
validPos (x, y) = x >= 0 && x < 4 && y >= 0 && y < 4

posOfNumInRow :: [Int] -> Int -> Int
posOfNumInRow (hd:tl) x = if hd == x then 0 else 1 + posOfNumInRow tl x

posOfNumInPuzzle :: Puzzle -> Int -> Pos
posOfNumInPuzzle (hd:tl) x = 
    if x `elem` hd
        then (0, (posOfNumInRow hd x))
    else
        (row + 1, col)
        where (row, col) = posOfNumInPuzzle tl x

shiftPos :: Move -> Pos -> Pos
shiftPos move (row, col) = 
    case move of 
        MUp -> (row - 1, col)
        MDown -> (row + 1, col)
        MLeft -> (row, col - 1)
        MRight -> (row, col + 1)

swapNumsInPuzzle :: Int -> Int -> Puzzle -> Puzzle
swapNumsInPuzzle x y rows = map (swapNumsInRow x y) rows

swapNumsInRow :: Int -> Int -> [Int] -> [Int]
swapNumsInRow x y row = map (swapNumOnTile x y) row

swapNumOnTile :: Int -> Int -> Int -> Int
swapNumOnTile x y tile = 
    case tile of
        n | n == x -> y
        n | n == y -> x
        n -> n

swapPosInPuzzle :: Pos -> Pos -> Puzzle -> Puzzle
swapPosInPuzzle (xrow, xcol) (yrow, ycol) puz = swapNumsInPuzzle x y puz
    where x = puz !! xrow !! xcol
          y = puz !! yrow !! ycol

doMoveOnPuzzle :: Move -> Puzzle -> Puzzle
doMoveOnPuzzle move puz = if (validPos newPos) then swapPosInPuzzle oldPos newPos puz else puz
    where oldPos = posOfNumInPuzzle puz 0
          newPos = shiftPos move oldPos

puzzleIsWon :: Puzzle -> Bool
puzzleIsWon puz = puz == initPuz

applyMoves :: [Move] -> Puzzle -> Puzzle
applyMoves [] puz = puz
applyMoves (m:ms) puz = applyMoves ms $ doMoveOnPuzzle m puz

moveList = [MUp, MDown, MLeft, MRight]

random0To3s :: RandomGen a => a -> [Int]
random0To3s g = randomRs (0, 3) g

genRandomMoves :: RandomGen a => a -> Int -> [Move]
genRandomMoves g n = map (moveList !!) (take n $ random0To3s g)

shuffle :: RandomGen a => a -> Int -> Puzzle -> Puzzle
shuffle g n puz = applyMoves moves puz
    where moves = genRandomMoves g n

numPrompt :: (Read b, Num b) => String -> IO b
numPrompt s = do
    putStr s
    hFlush stdout
    readLn

puzzleString :: Puzzle -> String
puzzleString puz = "╭──┬──┬──┬──╮\n" ++ (intercalate "\n├──┼──┼──┼──┤\n" $ map rowString puz) ++ "\n╰──┴──┴──┴──╯"

rowString :: [Int] -> String
rowString row = "│" ++ (intercalate "│" $ map tileString row) ++ "│"

tileString :: Int -> String
tileString n
    | n == 0 = "  "
    | n <= 9 = ' ' : (show n)
    | otherwise = show n

shuffleOnce :: IO ()
shuffleOnce = do
    g <- getStdGen
    n <- numPrompt "moves to attempt on the puzzle: "
    putStrLn $ puzzleString $ shuffle g n initPuz