{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Exercises where

import GHC.Base
import GHC.Float
import GHC.Generics (Generic)
import Generic.Random
import Test.QuickCheck
import Tyche

data Pair a b = Pair a b
  deriving (Eq, Show)

{-
Exercise: Write an Arbitrary instance for Pair a b, assuming a and b are
instances of Arbitrary.

(For now, you do not need to implement shrink, just arbitrary.)

Note: You can run `sample (arbitrary :: Gen (Pair Int Bool))` to get a sense of
your generator's output.

Optional extra exercise: Write a generator of Ints that always outputs 4.
-}

data CardinalDirection
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

{-
Exercise: Write an Arbitrary instance for CardinalDirection.

(For now, you do not need to implement shrink, just arbitrary.)
-}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show, Generic)

{-
Exercise: Write an Arbitrary instance for Tree a, assuming a is an instance of
Arbitrary.

(For now, you do not need to implement shrink, just arbitrary.)
-}

{-
Exercise: Improve the Arbitrary instance for Tree a using `frequency`.

(For now, you do not need to implement shrink, just arbitrary.)
-}

{-
Exercise: Improve the Arbitrary instance for Tree a using `sized`.

(For now, you do not need to implement shrink, just arbitrary.)
-}

{-
Exercise: Improve the Arbitrary instance for Tree a using `frequency`.

(For now, you do not need to implement shrink, just arbitrary.)
-}

{-
Exercise: Write shrink for Pair a b.

Do not use `subterms` or `recursivelyShrink`. (They require typeclasses that we
are not going to implement for this exercise.)
-}

{-
Exercise: Write shrink for Tree a.
Do not use `subterms` or `recursivelyShrink`.
-}

allTree :: (a -> Bool) -> Tree a -> Bool
allTree _ Leaf = True
allTree p (Node l x r) = p x && allTree p l && allTree p r

insertBST :: Int -> Tree Int -> Tree Int
insertBST x Leaf = Node Leaf x Leaf
insertBST x (Node l y r)
  | x < y = Node (insertBST x l) y r
  | x > y = Node l y (insertBST x r)
  | otherwise = Node l y r

member :: (Eq a) => a -> Tree a -> Bool
member _ Leaf = False
member x (Node l y r) = x == y || member x l || member x r

isBST :: Tree Int -> Bool
isBST Leaf = True
isBST (Node l x r) = allTree (< x) l && allTree (> x) r && isBST l && isBST r

sizeTree :: Tree a -> Int
sizeTree Leaf = 0
sizeTree (Node l _ r) = 1 + sizeTree l + sizeTree r

prop_insertPost :: Int -> Tree Int -> Property
prop_insertPost x t =
  Tyche.visualize "prop_insert_post" $
    labelNumber "size" (sizeTree t) $
      labelNumber "value" x $
        labelCategory "isLeaf" (show (t == Leaf)) $
          isBST t ==>
            member x (insertBST x t)

{-
https://tyche-pbt.github.io/tyche-extension/
-}

{-
Challenge: Write a generator for binary search trees.
-}
genBST :: (Int, Int) -> Gen (Tree Int)
genBST (lo, hi) | lo > hi = return Leaf
genBST (lo, hi) =
  frequency
    [ (1, return Leaf),
      ( 3,
        do
          x <- choose (lo, hi)
          l <- genBST (lo, x - 1)
          r <- genBST (x + 1, hi)
          return (Node l x r)
      )
    ]