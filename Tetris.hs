{- |
Module      : Tetrs
Description : The Tetris game (main module)
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : André Leufstedt, Oliver Björkblom, Kian Bergvall
Lab group   : 45
-}

module Main where

import ConsoleGUI
import Shapes
import Data.Maybe (isJust)
import Data.List (partition) 

--------------------------------------------------------------------------------
-- * The code that puts all the piece together
main :: IO ()
main = runGame tetrisGame

tetrisGame :: Game Tetris
tetrisGame = Game
  { startGame     = startTetris
  , stepGame      = stepTetris
  , drawGame      = drawTetris
  , gameInfo      = defaultGameInfo prop_Tetris
  , tickDelay     = defaultDelay
  , gameInvariant = prop_Tetris
  }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation
type Piece = (Pos, Shape)
type Pos   = (Int, Int)

-- | The state of the game consists of three parts:
data Tetris = Tetris
  { piece  :: Piece    -- ^ The position and shape of the falling piece
  , well   :: Shape    -- ^ The well (the playing field), where the falling pieces pile up
  , shapes :: [Shape]  -- ^ An infinite supply of random shapes
  }

-- | The size of the well
wellHeight, wellWidth  :: Int
wellHeight = 20
wellWidth  = 10

wellSize :: (Int, Int)
wellSize   = (wellHeight, wellWidth)

-- | Starting position for falling pieces
startPosition :: Pos
startPosition = (0, wellWidth `div` 2 - 1)

-- | Pos addition
add :: Pos -> Pos -> Pos
(h1, w1) `add` (h2, w2) = (h1 + h2, w1 + w2)

-- | Move the falling piece into position
place :: (Pos, Shape) -> Shape
place (v, s) = shiftShape v s

-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris piece well shape) = prop_Shape (snd piece) && wellSize == shapeSize well

-- | Adds one side of the walls
addWallSide1 :: (Int, Int) -> Shape -> Shape
addWallSide1 (y, x) (Shape h) =
  let width  = length (head h) + x
      emptyRow = replicate width $ Just Black
      newRowsAbove = replicate y emptyRow
      shiftShape = map (replicate x (Just Black) ++) h
  in Shape (newRowsAbove ++ shiftShape)

-- | Adds the other side of the walls
addWallSide2 :: (Int, Int) -> Shape -> Shape
addWallSide2 (y, x) (Shape h) =
  let width  = length (head h) + x
      emptyRow = replicate width $ Just Black
      newRowsBelow = replicate y emptyRow
      shiftShape = map (++ replicate x (Just Black)) h
  in Shape (shiftShape ++ newRowsBelow)

addWalls :: Shape -> Shape
addWalls s = addWallSide1 (1, 1) (addWallSide2 (1, 1) s)

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (pos, shapeType) well shapesArray) = let
    -- combine the well and the current falling piece
    well' = combine well $ place (pos, shapeType)
    -- Construct the walls
    in addWalls well'

-- | Movefunciton (y, x) Tetris -> Tetris
move :: (Int, Int) -> Tetris -> Tetris
move offset (Tetris (pos, shape) well shapes) =
    Tetris (newPos, shape) well shapes
  where 
    newPos = pos `add` offset

tick :: Tetris -> Maybe (Int, Tetris)
tick tetris 
  | collision tetris = Nothing
  | collision (move (1, 0) tetris) = dropNewPiece tetris
  | otherwise = Just (0, move (1, 0) tetris)

--Run this recursively until we hit the bottom (make it snap down)
snapDown :: Tetris -> Tetris
snapDown tetris
  | collision (move (1, 0) tetris) = tetris  
  | otherwise = snapDown (move (1, 0) tetris)  

-- | Modify moveDown to use snapDown
moveDown :: Tetris -> Maybe (Int, Tetris)
moveDown tetris = Just (0, snapDown tetris)

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well supply
 where
  well         = emptyShape wellSize
  piece:supply = createSupply rs 
   
createSupply :: [Double] -> [Shape]
createSupply rs = [allShapes !! (floor (r * 7)) | r <- rs]

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris action t 
  | action == MoveLeft = Just (0, (movePiece (-1) t))
  | action == MoveRight = Just (0, (movePiece (1) t))
  | action == Rotate = Just (0, (rotatePiece t)) 
  | action == MoveDown = moveDown t --Instead of "tick t" we can use this to make it snap down directly just like in real tetris
  | action == Tick = tick t 

collision :: Tetris -> Bool
collision (Tetris (pos, shape) well _) =
  let 
    (yPos, xPos) = pos
    (yShape, xShape) = shapeSize shape

    shiftedShape = shiftShape pos shape
    
    outofbounds = 
      yPos < 0 || (yPos + yShape >= wellHeight+1) ||
      xPos < 0 || (xPos + xShape >= wellWidth+1) 
    
    overlapswithwell = overlaps shiftedShape well

  in overlapswithwell || outofbounds

movePiece :: Int -> Tetris -> Tetris
movePiece dir t = 
  if dir == -1 || dir == 1 
  then (if collision (move (0, dir) t) then t else move (0, dir) t) 
  else t

rotate :: Tetris -> Tetris
rotate (Tetris (pos, shape) a b) = (Tetris (pos, newShape) a b) 
  where 
    newShape = rotateShape shape

rotatePiece :: Tetris -> Tetris
rotatePiece t = 
  if collision (rotate t) then t else rotate t  

dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris (pos, shape) well (first:shapes))  =  
  let
    newWell = combine well $ place (pos, shape)
    (n, nNWell) = clearLines newWell
    newPos = startPosition
    newShape = first
  in 
    Just (n, Tetris (newPos, newShape) nNWell shapes)

takeTopRows :: Shape -> Shape
takeTopRows well = Shape (take 2 (rows well))

checkRow :: Row -> Bool
checkRow x = all isJust x 

clearLines :: Shape -> (Int, Shape)
clearLines (Shape rows) =
  let
    width = case rows of  
      [] -> 0
      (r:_) -> length r
  
    (completeRows, incompleteRows) = partition checkRow rows 
    numCleared = length completeRows                      
    newEmptyRows = replicate numCleared (constructEmptyRow width)  
    newRows = newEmptyRows ++ incompleteRows
  in
    (numCleared, Shape newRows)  






  






























































