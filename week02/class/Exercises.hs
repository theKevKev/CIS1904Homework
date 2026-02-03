{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}
module Exercises where

import Data.List
import Test.HUnit
  ( Test (..),
    runTestTT,
    (~:),
    (~?=),
  )
import Test.QuickCheck

{- Example, not an exercise -}
example1 :: Int -> Int
example1 x = x * x

testExample1 :: Test
testExample1 =
  "example1"
    ~: [ "four" ~: example1 4 ~?= 16,
         "zero" ~: example1 0 ~?= 0
       ]

take :: Int -> [Int] -> [Int]
take i xs = undefined

{- Example -}
prop_prefix :: Int -> [Int] -> Bool
prop_prefix i xs = isPrefixOf (Data.List.take i xs) xs

sort :: [Int] -> [Int]
sort xs = undefined

{- Note: you do not need to implement the undefined functions; just write tests. -}
{- Describe, in English, two properties that should hold of the below sort
function.

FILL IN HERE

-}

listSort :: [Int] -> [Int]
listSort = undefined

{- Now write a _model-based_ property specifying the above function.
    Hint: you may use Data.List from the standard library. -}

{- Write a round-trip property for a function that parses a
string into an Int and a function that pretty prints the Int.
You can assume neither throws any exceptions. -}
parse :: String -> Int
parse = undefined

prettyPrint :: Int -> String
prettyPrint = undefined

{- Suppose we are implementing sets via the Haskell List type
   and want to make sure our library functions preserve a
   key set invariant: each element occurs in the set at most once.
   Write a property that ensures that foo maintains this invariant.
   (You may use `allDistinct`. ) -}
type MySet = [Int]

allDistinct :: [Int] -> Bool
allDistinct xs = Data.List.nub xs == xs

foo :: MySet -> MySet
foo = undefined

sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements xs ys = null (xs \\ ys) && null (ys \\ xs)

ordered :: [Int] -> Bool
ordered [] = True
ordered [x] = True
ordered (x1 : x2 : xs) = x1 <= x2 && ordered (x2 : xs)

{- Example -}
sortBad :: [Int] -> [Int]
sortBad [] = []
sortBad (p : xs) = sortBad lesser ++ [p] ++ sortBad greater
  where
    lesser = filter (< p) xs
    greater = filter (> p) xs
