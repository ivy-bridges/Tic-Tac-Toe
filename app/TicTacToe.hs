module TicTacToe where

import BoardHelper

import Data.Array
import Data.List


data Mark = X | O
    deriving (Eq, Show)

-- get opposing player    
opp :: Mark -> Mark
opp X = O
opp O = X

data Square = Filled Mark | Empty
    deriving (Eq, Show)
    
type Board = RectArray Square
type Move = (Mark, (Int, Int)) -- type of mark with coordinates of move


-- generate an empty w x h board
emptyBoard :: Int -> Int -> Board
emptyBoard w h = array ((1,1),(w,h)) [((x,y),Empty) | x <- [1..w], y <- [1..h]]


-- trying to mark an already filled square doest change the board
markSquare :: Board -> Move -> Board
markSquare board (mark, pos)
    | validMove = board//[(pos,Filled mark)]
    | otherwise = board
    where validMove = (board ! pos) == Empty

-- returns Nothing if no winner is found, or Just (winner's mark) if one is
checkWinner :: Board -> Maybe Mark
checkWinner board
    | any (all (==Filled X)) (getLines board) = Just X
    | any (all (==Filled O)) (getLines board) = Just O
    | otherwise = Nothing
    
    
newtype GameState = GameState (Board, Mark) deriving Eq

-- shows current player, then
-- formats state of the game like a tic tac toe grid
instance Show GameState where
    show (GameState (board, player)) = (show player) ++ "'s Turn\n" ++ boardString
        
        where boardString = unlines $ intersperse hline boardRows
              boardRows   = map (intersperse '║' . map squareName) (arrayRows board)
              hline = "═╬═╬═"
              
              squareName s 
                | s == Filled X = 'X'
                | s == Filled O = 'O'
                | otherwise     = ' '
                
              
    
-- given a position, marks the corresponding square and switches players
-- if the move is invalid, keep players the same
makeMove :: GameState -> (Int, Int) -> GameState
makeMove (GameState (board, player)) pos
    | validMove = GameState (markSquare board triedMove, opp player)
    | otherwise = GameState (board, player)
    where validMove = (board /= (markSquare board triedMove))
          triedMove = (player, pos)
    
    
    
-- pulls the board from a gamestate
pullBoard :: GameState -> Board
pullBoard (GameState (board, _)) = board    

    
    
    
    
    
    
    
    
    
    
    
    
    
    