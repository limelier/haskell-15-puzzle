module Lib
    ( runGame
    )
where

import           System.Random
import           System.IO
import           Data.List
import           UI.NCurses

type Puzzle = [[Int]]
puz :: Integral a => [[a]] -> Puzzle
puz [] = []
puz p  = map puzRow p
puzRow :: Integral a => [a] -> [Int]
puzRow []  = []
puzRow row = map fromIntegral row

initPuz = puz [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 0]]

data Move = MUp | MDown | MLeft| MRight deriving (Enum, Show);
type Pos = (Int, Int)

invertMove :: Move -> Move
invertMove MUp    = MDown
invertMove MDown  = MUp
invertMove MLeft  = MRight
invertMove MRight = MLeft

validPos :: Pos -> Bool -- allow only 0-3 for both pos
validPos (x, y) = x >= 0 && x < 4 && y >= 0 && y < 4

posOfNumInRow :: [Int] -> Int -> Int
posOfNumInRow (hd : tl) x = if hd == x then 0 else 1 + posOfNumInRow tl x

posOfNumInPuzzle :: Puzzle -> Int -> Pos
posOfNumInPuzzle (hd : tl) x = if x `elem` hd
    then (0, posOfNumInRow hd x)
    else (row + 1, col)
    where (row, col) = posOfNumInPuzzle tl x

shiftPos :: Move -> Pos -> Pos
shiftPos move (row, col) = case move of
    MUp    -> (row - 1, col)
    MDown  -> (row + 1, col)
    MLeft  -> (row, col - 1)
    MRight -> (row, col + 1)

swapNumsInPuzzle :: Int -> Int -> Puzzle -> Puzzle
swapNumsInPuzzle x y = map (swapNumsInRow x y)

swapNumsInRow :: Int -> Int -> [Int] -> [Int]
swapNumsInRow x y = map (swapNumOnTile x y)

swapNumOnTile :: Int -> Int -> Int -> Int
swapNumOnTile x y tile = case tile of
    n | n == x -> y
    n | n == y -> x
    n          -> n

swapPosInPuzzle :: Pos -> Pos -> Puzzle -> Puzzle
swapPosInPuzzle (xrow, xcol) (yrow, ycol) puz = swapNumsInPuzzle x y puz
  where
    x = puz !! xrow !! xcol
    y = puz !! yrow !! ycol

doMoveOnPuzzle :: Move -> Puzzle -> Puzzle
doMoveOnPuzzle move puz = if (validPos newPos)
    then swapPosInPuzzle oldPos newPos puz
    else puz
  where
    oldPos = posOfNumInPuzzle puz 0
    newPos = shiftPos move oldPos

puzzleIsWon :: Puzzle -> Bool
puzzleIsWon puz = puz == initPuz

applyMoves :: [Move] -> Puzzle -> Puzzle
applyMoves []       puz = puz
applyMoves (m : ms) puz = applyMoves ms $ doMoveOnPuzzle m puz

moveList = [MUp, MDown, MLeft, MRight]

random0To3s :: RandomGen a => a -> [Int]
random0To3s = randomRs (0, 3)

genRandomMoves :: RandomGen a => a -> Int -> [Move]
genRandomMoves g n = map (moveList !!) (take n $ random0To3s g)

shuffle :: RandomGen a => a -> Int -> Puzzle -> Puzzle
shuffle g n = applyMoves moves where moves = genRandomMoves g n

numPrompt :: (Read b, Num b) => String -> IO b
numPrompt s = do
    putStr s
    hFlush stdout
    readLn

puzzleString :: Puzzle -> String
puzzleString puz =
    "╭──┬──┬──┬──╮\n"
        ++ intercalate "\n├──┼──┼──┼──┤\n" (map rowString puz)
        ++ "\n╰──┴──┴──┴──╯"

rowString :: [Int] -> String
rowString row = "│" ++ intercalate "│" (map tileString row) ++ "│"

tileString :: Int -> String
tileString n | n == 0    = "  "
             | n <= 9    = ' ' : show n
             | otherwise = show n

-- shuffleOnce :: IO ()
-- shuffleOnce = do
--     g <- getStdGen
--     n <- numPrompt "moves to attempt on the puzzle: "
--     putStrLn $ puzzleString $ shuffle g n initPuz

updatePuzzle :: Window -> Puzzle -> Curses ()
updatePuzzle w puz = do
    updateWindow w $ do
        clear
        drawString $ puzzleString puz
    render
    if (puzzleIsWon puz)
        then do
            updateWindow w $ do
                moveCursor 10 0
                drawString "You win! Press Q to quit."
            render
            waitForQ w
        else do
            mMove <- waitForMove w
            case mMove of
                Just move -> updatePuzzle w (doMoveOnPuzzle move puz)
                Nothing   -> return ()


waitForMove :: Window -> Curses (Maybe Move)
waitForMove w = loop  where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing                  -> loop
            Just (EventSpecialKey k) -> case k of
                KeyUpArrow    -> return $ Just MDown
                KeyDownArrow  -> return $ Just MUp
                KeyLeftArrow  -> return $ Just MRight
                KeyRightArrow -> return $ Just MLeft
                _             -> loop
            Just (EventCharacter c) -> case c of
                'q' -> return Nothing
                _   -> loop
            _ -> loop

waitForQ :: Window -> Curses ()
waitForQ w = loop  where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing                 -> loop
            Just (EventCharacter c) -> case c of
                'q' -> return ()
                _   -> loop
            _ -> loop

runGame :: IO ()
runGame = do
    g <- getStdGen
    runCurses
        $ let puzzle = shuffle g 300 initPuz
          in  do
                  setEcho False
                  setCursorMode CursorInvisible
                  w <- defaultWindow
                  updatePuzzle w puzzle
