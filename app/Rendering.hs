module Rendering where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import TicTacToe
-- 

{- gloss play function is set up as
play :: Display -- (InWindow "Tic-Tac-Toe" (500,500), (10,10))
     -> Color   -- background color
     -> Int     -- number of steps to take/second
     -> world   -- initial state of the world
     -> (world -> Picture) 
     -> (Event -> world -> world)
     -> (Float -> world -> world) -- we can just pass this
     -> IO ()
-}

-- hold state of game as well as currently selected square
type GameWorld = (GameState, (Int, Int))

isArrowKey :: SpecialKey -> Bool
isArrowKey key = elem key [KeyUp,KeyDown,KeyLeft,KeyRight]

isSpaceKey :: SpecialKey -> Bool
isSpaceKey key = key == KeySpace

moveSelection :: SpecialKey -> (Int, Int) -> (Int, Int)
moveSelection key (x,y)
    | key == KeyDown    && y>1 = (x,y-1)
    | key == KeyUp      && y<3 = (x,y+1)
    | key == KeyLeft    && x>1 = (x-1,y)
    | key == KeyRight   && x<3 = (x+1,y)
    | otherwise                = (x,y)

-- handling key events
-- arrow keys move the selection around, space key makes a move (marks a square)
handleEvents :: Event -> GameWorld -> GameWorld
handleEvents (EventKey (SpecialKey key) Down _ _) (state, selection)
    | isArrowKey key = (state, moveSelection key selection)
    | isSpaceKey key = (makeMove state selection, selection)
handleEvents _ game = game

passTime :: Float -> GameWorld -> GameWorld
passTime _ world = world

renderGame :: GameWorld -> Picture
renderGame (state, (sx,sy)) = Pictures [gridLines, squareHighlight]
    where gridLines = Pictures $ map (Color black) [l1,l2,l3,l4]
    
          l1 = Line [(-75,-225), (-75, 225)]
          l2 = Line [(75,-225), (75, 225)]        
          l3 = Line [(-225,-75), (225, -75)]
          l4 = Line [(-225,75), (225, 75)]
          
          squareX = (-300) + (150*fromIntegral(sx)) 
          squareY = (-300) + (150*fromIntegral(sy)) :: Float
          selectionSquare = Color selectionColor $ rectangleSolid 149 149
          
          squareHighlight = Translate squareX squareY selectionSquare
          
          
          
          
          
          
          
          
-- useful colors
selectionColor = makeColor 0.5 0.5 1 0.5