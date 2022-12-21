module BoardHelper (arrayRows, getLines, RectArray) where
-- helper functions for rectangular arrays (arrays indexed with (Int, Int) values)


import Data.Array
import Data.List

type RectArray a = Array (Int, Int) a

-- gets all contiguous subsequences of length num from a list
-- so (chunks 2) [1,2,3,4] would give [[1,2],[2,3],[3,4]]
chunks :: Int -> [a] -> [[a]]
chunks num vals = map (reverse . take num . reverse) (drop num $ inits vals)


-- functions for getting the rows and columns of an array
-- in the form of a list of lists of elements
arrayRows :: RectArray a -> [[a]]
arrayRows rarray = map (\y -> [rarray ! (x,y) | x <- [1..width]]) [1..height]
    where (width, height) = snd (bounds rarray)

arrayCols :: RectArray a -> [[a]]
arrayCols rarray = map (\x -> [rarray ! (x,y) | y <- [1..height]]) [1..width]
    where (width, height) = snd (bounds rarray)
    


-- get diagonals going towards the up-right          
arrayDiags :: RectArray a -> [[a]]
arrayDiags rarray = map (\d -> [rarray ! (x,y) | x <- [1..width], y <- [1..height], x+y==d]) [1..width+height]
    where (width, height) = snd (bounds rarray)
    
-- get diagonals going towards the up-left by flipping the x coords
arrayDiags' :: RectArray a -> [[a]]
arrayDiags' rarray = map (\d -> [rarray' ! (x,y) | (x,y) <- indices rarray, x+y==d]) [1..width+height]
    where (width, height) = snd (bounds rarray)
          rarray' = rarray//[((x,y),rarray ! (width+1-x,y)) | (x,y) <- indices rarray]

-- gets all lines from the board (rows, columns, main diagonals)
getLines :: RectArray a -> [[a]]
getLines board = rows ++ cols ++ rightDiag ++ leftDiag
    where rows = (arrayRows board)
          cols = (arrayCols board)
          
          rightDiag = filter ((==width) . length) (arrayDiags  board)
          leftDiag  = filter ((==width) . length) (arrayDiags' board)
          
          (width, height) = snd (bounds board)
      