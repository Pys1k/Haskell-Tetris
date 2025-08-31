{- |
Module      : Shapes
Description : Types and functions for shapes. The list of all tetris pieces.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : André Leufstedt, Kian Bergvall, Oliver Björkblom
Lab group   : 45
-}

module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Test.QuickCheck


-- * Shapes

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

type Square = Maybe Colour

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
type Row   = [Square]
data Shape = Shape [Row] deriving Eq

rows :: Shape -> [Row]
rows (Shape rs) = rs

-- * Showing shapes
showShape :: Shape -> String
showShape (Shape rows) = unlines [showRow r | r <- rows]
 where
  showRow r = [showSquare s | s <- r]

  showSquare Nothing      = '.'
  showSquare (Just Black) = '█' -- can change to '█' on linux/mac
  showSquare (Just Grey)  = '▓' -- can change to '▓'
  showSquare (Just c)     = case show c of [] -> 'c'; x:_ -> x

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [Shape (makeSquares s) | s <- shapes]
 where
   makeSquares = map (map colour)
   colour c    = lookup c [ ('I', Red),  ('J', Grey),  ('T', Blue), ('O', Yellow)
                          , ('Z', Cyan), ('L', Green), ('S', Purple) ]
   shapes = [["I",
              "I",
              "I",
              "I"],
             [" J",
              " J",
              "JJ"],
             [" T",
              "TT",
              " T"],
             ["OO",
              "OO"],
             [" Z",
              "ZZ",
              "Z "],
             ["LL",
              " L",
              " L"],
             ["S ",
              "SS",
              " S"]]

-- * Some simple functions

-- ** A1
emptyShape :: (Int, Int) -> Shape
emptyShape (y, x) = Shape (replicate y (replicate x Nothing))

constructEmptyRow :: Int -> Row
constructEmptyRow y = replicate y Nothing

-- ** A2
-- | The size (height and width) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (Shape x) 
  | null x = (0, 0)
  | otherwise = (length x, length (head x))


-- ** A3

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (Shape x) = length (concatMap (filter isJust) x)
  where
    isJust Nothing = False
    isJust _       = True


-- * The Shape invariant

-- ** A4
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (Shape x) 
  | null x  = False
  | otherwise = let 
      lengthCheck = not (null (head x)) && not (null x) && (length (concat x) == (length (head x) * length x))
      rowCheck = checkEachRow (Shape x) 0
      in  lengthCheck && rowCheck


checkEachRow :: Shape -> Int -> Bool
checkEachRow (Shape x) n
  | n > (length x-1) = True 
  | null (x !! n) = False
  | otherwise = checkEachRow (Shape x) (n+1)


-- * Test data generators

-- ** A5
-- | A random generator for colours
genColour :: Gen Colour
genColour = elements [minBound .. maxBound]


instance Arbitrary Colour where
  arbitrary = genColour

-- ** A6
-- | A random generator for shapes
genShape :: Gen Shape
genShape = elements allShapes


instance Arbitrary Shape where
  arbitrary = genShape

-- * Transforming shapes
-- ** A7
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (Shape rows) = Shape . reverse . transpose $ rows

-- ** A8
-- | shiftShape (y) adds empty squares above and to the left of the shape
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (y, x) (Shape h) = 
  let width  = length (head h) + x
      emptyRow = replicate width Nothing
      newRowsAbove = replicate y emptyRow
      shiftShape = map (replicate x Nothing ++) h
  in Shape (newRowsAbove ++ shiftShape)

-- ** A9
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (y, x) (Shape h) = 
  let width  = length (head h) + x 
      emptyRow = replicate width Nothing
      newRowsBelow = replicate y emptyRow
      shiftShape = map (++ replicate x Nothing) h
  in Shape (shiftShape ++ newRowsBelow)

  
-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (y,x) (Shape h) = 
  let (currentY, currentX) = shapeSize (Shape h)
      missingY = max 0 (y - currentY) --This eliminates the chance of it being negative
      missingX = max 0 (x - currentX) 
  in padShape (missingY, missingX) (Shape h)

-- * Comparing and combining shapes'

-- ** B1
-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
overlaps (Shape []) _ = False
overlaps _ (Shape []) = False
overlaps (Shape (r1:rs1)) (Shape (r2:rs2)) = rowsOverlap r1 r2 || overlaps (Shape rs1) (Shape rs2)

rowsOverlap :: Row -> Row -> Bool
rowsOverlap [] _ = False
rowsOverlap _ [] = False
rowsOverlap (s1:ss1) (s2:ss2) = (s1 /= Nothing && s2 /= Nothing) || rowsOverlap ss1 ss2


-- ** B2
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith f (Shape shape1) (Shape shape2) = Shape $ zipWith (zipWith f) shape1 shape2

-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = let
  h_maxsize = max(fst $ shapeSize s1) (fst $ shapeSize s2)
  w_maxsize = max(snd $ shapeSize s1) (snd $ shapeSize s2) 

  paddedShape1 = padShapeTo (h_maxsize, w_maxsize) s1
  paddedShape2 = padShapeTo (h_maxsize, w_maxsize) s2
  in zipShapeWith combineSquares paddedShape1 paddedShape2

combineSquares :: Square -> Square -> Square
combineSquares Nothing sq = sq
combineSquares sq Nothing = sq
combineSquares sq _       = sq