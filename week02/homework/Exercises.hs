module Exercises where

import Test.HUnit
  ( Test (..),
    Testable (..),
    assertBool,
    runTestTT,
    (~:),
    (~?=),
  )
import Test.QuickCheck

{- Homework 2: Intro to Testing -}

{- Exercise 0
For each function description below, write a specification in English for what
it means for an implementation to be correct. If you think a function should
throw some kind of error on some inputs, indicate for which inputs it should do
so. (You may also choose some default behavior for such inputs.)
You are being graded on content, not grammar, so bullet points are ok.

0a. A function that reverses a list

FILL IN HERE

0b. A function that inserts an integer into a list of integers at a given
    natural number index

FILL IN HERE

0c. A function that factors an integer into its prime factors

FILL IN HERE

-}

{- For some of the following exercises, you will likely not be able to
completely specify the problem with the given number of tests.
That is ok, but be as thorough as possible. For any cases where you
previously wrote that a given input should throw an error, we recommend
specifying (and testing for) some default behavior for these exercises.

You do not need to implement the given function under test; it is just there so
you have a function to reference in your tests.

You may NOT add any imports except where indicated. -}

{- Exercise 1
Write one property-based test and one unit test case to test the
list reverse function below. -}

listReverse :: [Int] -> [Int]
listReverse xs = undefined

{- Exercise 2
Write either:
a) one property-based test and two unit tests
OR
b) two property-based tests
to test the list insert function below

For this exercise only, you may import and use any function from
Data.List except `insert` and `insertBy`:
https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-List.html

To make the standard library functions easier to use, we have changed the index
type from a natural number to Int. Why did we have it as a natural number in
Exercise 0? Consider how the type eliminates a category of invalid inputs
that we now need to test for.
-}
listInsert :: [Int] -> Int -> Int -> [Int]
listInsert xs x i = undefined

{- Exercise 3
To specify the prime factorization function below, either
a) write one property-based test and two unit tests
OR
b) write one property-based test and one unit test, and change the input type

For this exercise only, you may import and use `product` from Data.List.
-}
primeFactors :: Int -> [Int]
primeFactors x = undefined

{-
Write down the number of hours it took you to complete this homework. Please
also write one question or comment about any of the material we have covered so
far.
-}

time :: Double
time = error "unimplemented"

question :: String
question = error "unimplemented"