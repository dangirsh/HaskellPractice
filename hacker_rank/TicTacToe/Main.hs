-- Tic tac toe AI for HackerRank Challenge 2
-- Dan Girshovich, 1/13

module Main where

import Data.List (sort)

-- 3 Given Helpers --
main = do

    -- If player is X, I'm the first player.
    -- If player is O, I'm the second player.
    player <- getLine

    -- Read the board now. The board is a list of strings filled with X, O or _.
    board <- getList 3

    -- Proceed with processing and print 2 integers separated by a single space.
    putStrLn.(\(x, y) -> show x ++ " " ++ show y).nextPosition player $ board

-- Reads the input.
getList :: Int -> IO [String]
getList n =
    if n==0 then
        return []
    else do
        i <- getLine;
        is <- getList(n-1);
        return (i:is)

-- player == "X" or "O" representing the next move to be made.
-- board is a list of strings with 'X', 'O' or '_' :- the current state of the board
-- return a pair (x,y) which is your next move
nextPosition :: String -> [String] -> Position
nextPosition playerStr boardStr = nextPosition' player board
    where
        board = toBoard boardStr
        player = toPlayer $ head playerStr


-- Begin my code --

type Position = (Int, Int)
data Player = X | O | Neither deriving (Eq, Ord)
type Board = Position -> Player

instance Show Player where
    show X = "X"
    show O = "O"
    show Neither = "_"

-- The input board must be (n-1) x (n-1)
n = 2

-- Player helpers --

toPlayer :: Char -> Player
toPlayer 'X' = X
toPlayer 'O' = O
toPlayer _ = Neither

-- With this, need to maintain invariant that Min player wants small ordinals
-- and Max player wants large ordinals (notjust Ints). See nextPosition'.
playerFunc :: Ord a => Player -> ([a] -> a)
playerFunc X = maximum
playerFunc O = minimum
playerFunc _ = error "Only X and O have player functions!"

playerVal :: Player -> Int
playerVal X = 1
playerVal O = -1
playerVal Neither = 0

otherPlayer :: Player -> Player
otherPlayer Neither = Neither
otherPlayer X = O
otherPlayer O = X


-- Board helpers --

toBoard :: [String] -> Board
toBoard boardStr = \(r, c) -> toPlayer (boardStr !! r !! c)

makeBoardStr :: Board -> String
makeBoardStr b = init $ concat $ rowMap ((flip (++) "\n") . (concatMap show)) b

transposeBoard :: Board -> Board
transposeBoard b = \(r, c) -> b (c, r)

rowMap :: ([Player] -> a) -> Board -> [a]
rowMap f b = map (f . rowAsList) [0..n]
    where
        rowAsList r = map (\c -> b (r, c)) [0..n]

diag1 :: Board -> [Player]
diag1 b = [b (r, c) | r <- [0..n], c <- [0..n], r == c]

diag2 :: Board -> [Player]
diag2 b = [b (r, c) | r <- [0..n], c <- [0..n], r + c == n]

-- Early game optimizations --

blankBoard :: Board -> Bool
blankBoard b = all ((== Neither) . b) [(r, c) | r <- [0..n], c <- [0..n]]

firstMove = (0, 0)

-- Game Logic --

nextPosition' :: Player -> Board -> Position
nextPosition' p b =
    if blankBoard b then
        firstMove
    else if null res then
        error "Board is full!"
    else snd $ playerFunc p $ res
    where
        res = [(search (otherPlayer p) (addPos b p pos), pos) | pos <- openPoses b]

-- Minimax search
search :: Player -> Board -> Int
search p b =
    if isWinning p b then
        playerVal p
    else if null nextBoards then
        playerVal Neither
    else
        playerFunc p $ map (search (otherPlayer p)) nextBoards
    where
        nextBoards = map (addPos b p) (openPoses b)

-- True if the board is in a winning state for the player.
-- False otherwise.
isWinning :: Player -> Board -> Bool
isWinning p = inLine [p, p, p]

addPos :: Board -> Player -> Position -> Board
addPos b p pos = \pos' -> if pos == pos' then p else b pos'

openPoses :: Board -> [Position]
openPoses  b = [(r, c) | r <- [0..n], c <- [0..n], b (r, c) == Neither]

-- True if the board contains the set of players in a line.
-- False otherwise.
inLine :: [Player] -> Board -> Bool
inLine ps b = checkHoriz || checkVert || checkDiag
    where
        checkHoriz = checkRows b
        checkVert = checkRows (transposeBoard b)
        checkDiag = checkThree (diag1 b) || checkThree (diag2 b)
        checkRows = or . (rowMap checkThree)
        checkThree = ((==) (sort ps)) . sort