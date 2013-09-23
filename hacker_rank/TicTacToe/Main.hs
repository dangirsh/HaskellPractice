-- Tic tac toe AI for HackerRank Challenge 2
-- Dan Girshovich, 1/13

module Main where

import Data.List (sort)

-- 3 Given Helpers --
main = do
    player <- getLine
    board <- getList 3
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

nextPosition :: String -> [String] -> Position
nextPosition playerStr boardStr =
    if isEmpty board then -- early game optimization. fix after adding memoization
        firstMove
    else
        snd $ nextPosition' player board
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

-- The input board must be (n+1) x (n+1)
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
otherPlayer X = O
otherPlayer O = X
otherPlayer Neither = Neither


-- Board helpers --

toBoard :: [String] -> Board
toBoard boardStr = \(r, c) -> toPlayer (boardStr !! r !! c)

makeBoardStr :: Board -> String
makeBoardStr = init . concat . (rowMap ((++ "\n") . (concatMap show)))

transposeBoard :: Board -> Board
transposeBoard b = \(r, c) -> b (c, r)

rowMap :: ([Player] -> a) -> Board -> [a]
rowMap f b = [f $ getRow r | r <- [0..n]]
    where
        getRow r = [b (r, c) | c <- [0..n]]

boardMap :: (Player -> a) -> Board -> [a]
boardMap f b = concat (rowMap (map f) b)

allPoses :: [Position]
allPoses = [(r, c) | r <- [0..n], c <- [0..n]]

diag1 :: Board -> [Player]
diag1 = (flip map) $ filter (\(r, c) -> r == c) allPoses

diag2 :: Board -> [Player]
diag2 = (flip map) $ filter (\(r, c) -> r + c == n) allPoses

-- Early game optimizations --

isEmpty :: Board -> Bool
isEmpty = and . (boardMap (== Neither))

firstMove = (0, 0)

-- Game Logic --

-- Starts the minimax search
nextPosition' :: Player -> Board -> (Int, Position)
nextPosition' p b =
    (playerFunc p) (zip moveScores (openPoses b))
    where
        moveScores = map (minimaxVal p) (nextBoards b p)

minimaxVal :: Player -> Board -> Int
minimaxVal p b =
    if winningPlayer b /= Neither then -- win leaf
        playerVal $ winningPlayer b
    else if isFull b then -- tie leaf
        playerVal Neither
    else -- node (game not over)
        fst $ nextPosition' (otherPlayer p) b

-- returns all possible boards that a player can move to
nextBoards :: Board -> Player -> [Board]
nextBoards b p = map (addPos b p) (openPoses b)

winningPlayer :: Board -> Player
winningPlayer b =
    if inLine [X, X, X] b then
        X
    else if inLine [O, O, O] b then
        O
    else
        Neither

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


-- debug --

-- evaluates the final board and moves produced with the given first move
playGame :: Position -> (Board, [Position])
playGame firstMove =
    play X (addPos blankBoard O firstMove) [firstMove]
    where
        blankBoard = \p -> Neither
        gameOver b = (winningPlayer b) /= Neither || isFull b
        play p b moves =
            if gameOver b then
                (b, moves)
            else
                let move = snd $ nextPosition' p b in
                play (otherPlayer p) (addPos b p move) (move:moves)


isFull :: Board -> Bool
isFull b = and $ boardMap (Neither /=) b

isTie :: Board -> Bool
isTie b = isFull b && winningPlayer b == Neither

debug :: [(String, [Position])]
debug =
    let boards = map playGame allPoses in
    let tied_boards = filter (not . isTie . fst) boards in
    map (\(b, ms) -> (makeBoardStr b, ms)) tied_boards
