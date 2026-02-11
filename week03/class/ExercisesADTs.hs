module Exercises where

data Tree
  = Leaf
  | Node Tree Int Tree
  deriving (Show)

{- Exercise:
   Write a function isLeaf that checks if a tree is a leaf. -}

isLeaf :: Tree -> Bool
isLeaf x = case x of
  Leaf -> True
  Node {} -> False

{- Exercise:
   Write a function getRootVal that returns the integer value
   at the root of the tree. -}

getRootVal :: Tree -> Int
getRootVal Leaf = -1
getRootVal (Node _ x _) = x

--------------

data Coord = Coord Int Int
  deriving (Show)

c1 :: Coord
c1 = Coord 90 0

c2 :: Coord
c2 = Coord 0 90

{- Exercise:
   Write a function addCoord that (point-wise) adds two coordinates,
   e.g. addCoord c1 c2 should be Coord 90 90. -}

addCoord :: Coord -> Coord -> Coord
addCoord (Coord c1x c1y) (Coord c2x c2y) = Coord (c1x + c2x) (c1y + c2y)