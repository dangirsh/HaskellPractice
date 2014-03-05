{-# LANGUAGE ParallelListComp #-}

import System.Environment (getArgs)
import Control.Applicative ((<$>), (<*>))
import Foreign.Marshal.Utils (fromBool)
import Data.List (intersperse)

main :: IO ()
main = do
    args <- getArgs
    let fname = if null args then "init.state" else head args
    state <- parseState <$> readFile fname
    mapM_ print $ take 50 (propagate state)

propagate :: State -> [State]
propagate = iterate step


step :: State -> State
step state = State $ [[update i j | j <- [0..m-1]] | i <- [0..n-1]]
    where
        update i j | neighbors i j < 2 || neighbors i j > 3 = False
                   | not (getNode state i j) && neighbors i j == 3 = True
                   | otherwise = getNode state i j
        neighbors i j = sum . map fromBool $ [getNode state i' j' | i' <- [i-1..i+1]
                                                                  , j' <- [j-1..j+1]
                                                                  , (i', j') /= (i, j)
                                                                  , i' >= 0
                                                                  , j' >= 0
                                                                  , i' < n
                                                                  , j' < m]
        n = length $ getState state
        m = length $ getState state !! 0


getNode :: State -> Int -> Int -> Node
getNode state i j = getState state !! i !! j


type Node = Bool


data State = State {getState :: [[Node]]}


instance Show State where

    show = unlines . map (map toChar) . getState
        where
            toChar True = '0'
            toChar False = '_'


parseState :: String -> State
parseState stateStr = State $ map (map toBool) $ lines stateStr
    where
        toBool '0' = True
        toBool '_' = False
        toBool c = error ("Invalid character: " ++ [c])

