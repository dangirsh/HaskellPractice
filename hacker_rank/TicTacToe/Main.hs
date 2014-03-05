-- Tic tac toe AI for HackerRank Challenge 2
-- Dan Girshovich, 1/13

module Main where

import Data.List (sort)
import qualified Data.Map as Map

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
    snd $ nextPosition' Map.empty player board
    where
        board = toBoard boardStr
        player = toPlayer $ head playerStr

-- Begin my code --

type Position = (Int, Int)
type ScoredPosition = (Int, Position)
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

playerFunc :: Ord a => Player -> ([a] -> a)
playerFunc X = maximum
playerFunc O = minimum
playerFunc Neither = error "Only X and O have player functions!"

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

allPositions :: [Position]
allPositions = [(r, c) | r <- [0..n], c <- [0..n]]

diag1 :: Board -> [Player]
diag1 b = map b $ filter (\(r, c) -> r == c) allPositions

diag2 :: Board -> [Player]
diag2 b = map b $ filter (\(r, c) -> r + c == n) allPositions

isFull :: Board -> Bool
isFull = and . (boardMap (/= Neither))

isEmpty :: Board -> Bool
isEmpty = and . (boardMap (== Neither))

-- Game Logic --

-- Starts the minimax search
nextPosition' :: Map.Map String ScoredPosition -> Player -> Board -> ScoredPosition
nextPosition' m p b =
    let bStr = makeBoardStr b in
    case Map.lookup bStr m of
        Just memVal -> memVal
        Nothing ->
            let moveScores = foldl (minimaxVal p) m (nextBoards b p) in
            let bestScoredPos = (playerFunc p) $ zip moveScores (openPosition b)
            let newM = Map.insert bStr bestScoredPos m

minimaxVal :: Player -> Board -> Int
minimaxVal m b
    | winningPlayer b /= Neither    -> playerVal $ winningPlayer b
    | isFull b                      -> playerVal Neither
    | otherwise                     -> fst $ nextPosition' m (otherPlayer p) b

-- returns all possible boards that a player can move to
nextBoards :: Board -> Player -> [Board]
nextBoards b p = map (addPosition b p) (openPosition b)

winningPlayer :: Board -> Player
winningPlayer b = if hasWon X b then X else if hasWon O b then O else Neither

addPosition :: Board -> Player -> Position -> Board
addPosition b p pos = \pos' -> if pos == pos' then p else b pos'

openPosition :: Board -> [Position]
openPosition  b = [(r, c) | r <- [0..n], c <- [0..n], b (r, c) == Neither]

-- True if the board contains n of the given player in a line.
-- False otherwise.
hasWon :: Player -> Board -> Bool
hasWon p b = inLineHoriz || inLineVert || inLineDiag
    where
        inLineN = (replicate n p ==)
        inLineHoriz = or . (rowMap inLineN)
        inLineVert = inLineHoriz (transposeBoard b)
        inLineDiag = inLineN (diag1 b) || inLineN (diag2 b)


-- debug --

-- evaluates the final board and moves produced with the given first move
--playGame :: Position -> (Board, [Position])
--playGame firstMove =
--    play X (addPosition blankBoard O firstMove) [firstMove]
--    where
--        blankBoard = \p -> Neither
--        gameOver b = (winningPlayer b) /= Neither || isFull b
--        play p b moves =
--            if gameOver b then
--                (b, moves)
--            else
--                let move = snd $ nextPosition' p b in
--                play (otherPlayer p) (addPosition b p move) (move:moves)

--isTie :: Board -> Bool
--isTie b = isFull b && winningPlayer b == Neither

--debug :: [(String, [Position])]
--debug =
--    let boards = map playGame allPositions in
--    let tied_boards = filter (not . isTie . fst) boards in
--    map (\(b, ms) -> (makeBoardStr b, ms)) tied_boards
