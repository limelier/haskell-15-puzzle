module Lib
    ( runGame
    )
where

import           System.Random
import           System.IO
import           Data.List
import           Data.Maybe
import qualified Data.PQueue.Prio.Min as PQ
import           UI.NCurses
import           Control.Concurrent
import           Control.Monad.IO.Class

type Puzzle = [[Int]]
puz :: Integral a => [[a]] -> Puzzle
puz [] = []
puz p  = map puzRow p
puzRow :: Integral a => [a] -> [Int]
puzRow []  = []
puzRow row = map fromIntegral row

initPuz = puz [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 0]]

data Move = MUp | MDown | MLeft | MRight deriving (Enum, Show);
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

tryMoveOnPuzzle :: Move -> Puzzle -> Maybe Puzzle
tryMoveOnPuzzle move puz = if (validPos newPos)
    then Just (swapPosInPuzzle oldPos newPos puz)
    else Nothing
  where
    oldPos = posOfNumInPuzzle puz 0
    newPos = shiftPos move oldPos

doMoveOnPuzzle :: Move -> Puzzle -> Puzzle
doMoveOnPuzzle move puz = case tryMoveOnPuzzle move puz of
    Nothing -> puz
    Just puz' -> puz'

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
            (mMove, mChar) <- waitForMove w
            case mMove of
                Just move -> updatePuzzle w (doMoveOnPuzzle move puz)
                Nothing   -> case mChar of
                    Just 's' -> solveAndQuit w puz
                    _ -> return ()

solveAndQuit :: Window -> Puzzle -> Curses ()
solveAndQuit w puz = do
    showOneSecondApart w puzs
    return ()
    where puzs = solve' puz

showOneSecondApart :: Window -> [Puzzle] -> Curses ()
showOneSecondApart w [] = do return () -- or "do waitForQ w" to delay
showOneSecondApart w (p:ps) = do
    updateWindow w $ do
        clear
        drawString $ puzzleString p
    render
    liftIO $ threadDelay 1000000
    showOneSecondApart w ps

waitForMove :: Window -> Curses ((Maybe Move, Maybe Char))
waitForMove w = loop  where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing                  -> loop
            Just (EventSpecialKey k) -> case k of
                KeyUpArrow    -> return $ (Just MDown, Nothing)
                KeyDownArrow  -> return $ (Just MUp, Nothing)
                KeyLeftArrow  -> return $ (Just MRight, Nothing)
                KeyRightArrow -> return $ (Just MLeft, Nothing)
                _             -> loop
            Just (EventCharacter c) -> case c of
                'q' -> return $ (Nothing, Just 'q')
                's' -> return $ (Nothing, Just 's')
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
        $ let puzzle = shuffle g 30 initPuz
          in  do
                  setEcho False
                  setCursorMode CursorInvisible
                  w <- defaultWindow
                  updatePuzzle w puzzle


-- heuristic: sum of, for each tile, manhattan distance to final position
heuristic :: Puzzle -> Int
heuristic puz = sum $ map score [0 .. 15]  where
    score num =
        let (wx, wy) = posOfNumInPuzzle initPuz num
            (x , y ) = posOfNumInPuzzle puz num
        in  abs (wx - x) + abs (wy - y)

data GameState = GameState {
    puzz :: Puzzle,
    dist :: Int,
    moves :: Int,
    prev :: Maybe GameState
} deriving (Show, Eq, Ord)

puzzlesTo :: GameState -> [Puzzle]
puzzlesTo state = reverse $ revPuzzlesTo state
    where revPuzzlesTo st = case prev st of
                                Nothing -> [puzz st]
                                Just st' -> puzz st : revPuzzlesTo st'

type Frontier = PQ.MinPQueue Int GameState

mkGameState :: Puzzle -> GameState
mkGameState p = GameState p h 0 Nothing
    where
        h = heuristic p

neighbor :: GameState -> Move -> Maybe GameState
neighbor state m =
    let p = puzz state in
        case tryMoveOnPuzzle m (puzz state) of
            Nothing -> Nothing
            Just p' -> Just (state {
                puzz = p',
                dist = heuristic p',
                moves = moves state + 1,
                prev = Just state
            })

neighbors :: GameState -> [GameState]
neighbors st = mapMaybe (neighbor st) [MUp, MDown, MLeft, MRight]

solve :: Puzzle -> GameState
solve p = go (PQ.fromList [(dist st, st)])
    where 
        st = mkGameState p
        go fr = if dist state == 0
                    then state
                    else go fr2
            where
                -- get most promising game state in frontier
                ((_, state), fr1) = PQ.deleteFindMin fr


                -- filter old state from neighbors, do not 
                -- go backwards
                nbors = case prev state of
                    Nothing -> neighbors state
                    Just pre -> filter (\x -> puzz x /= puzz pre)
                                    (neighbors state)
                
                -- priority of puz = moves + heuristic
                pairs = zip [moves nb + dist nb | nb <- nbors] nbors
                fr2 = foldr (uncurry PQ.insert) fr1 pairs

solve' :: Puzzle -> [Puzzle]
solve' p = puzzlesTo (solve p)
