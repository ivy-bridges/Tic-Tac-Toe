module Main where

import Graphics.Gloss
import TicTacToe
import Rendering

main :: IO ()
main = do
    
    let initialBoard = emptyBoard 3 3
        initialState = GameState (initialBoard, X)
        initialWorld = (initialState, (2,2))
        
    play
        (InWindow "Tic-Tac-Toe" (500,500) (10,10))
        white
        1
        initialWorld
        renderGame
        handleEvents
        passTime
        
        
