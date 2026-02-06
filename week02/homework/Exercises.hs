module Exercises where

import Data.List (isSubsequenceOf, product)
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

\* The function should return the same list but with every element in the opposite position of what it was before
\* no error inputs unless the list is not a list but that's a compile check.
\* some properties include:
  * every element being in the returned list (with the same count)
  * each element is in the opposite position of the array in the returned list
  * returned list is the same length
\* some base cases include:
  * empty list returns empty list
  * singleton list returns the same singleton list

0b. A function that inserts an integer into a list of integers at a given
    natural number index

\* should throw an error if the index would place the integer beyond the possible indices (0 to N)
\* should error if index is not a natural number
\* it should keep all integers before that index unchanged and all integers after that index should be increased by one index
\* that inserted integer should be at that index
\* the list should have it's length larger by exactly 1

0c. A function that factors an integer into its prime factors

\* should return a list of prime factors (integers)
\* there may be duplicates of the prime factors if that integer has multiplicity of that factor greater than 1
\* negative inputs should be handled appropriately with just the factors of it's aboslute value
\* prime numbers should just return a list of that prime, 1 as well.

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

test_empty_list :: Test
test_empty_list = "empty_list" ~: [] ~?= listReverse []

listLength :: [Int] -> Int
listLength [] = 0
listLength (x : xs) = 1 + listLength xs

prop_same_length :: [Int] -> Bool
prop_same_length xs = listLength xs == listLength (listReverse xs)

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

prop_negative_index :: [Int] -> Int -> Int -> Property
prop_negative_index xs x i = i < 0 ==> length (listInsert xs x i) == length xs

prop_same_elements :: [Int] -> Int -> Int -> Bool
prop_same_elements xs x i = isSubsequenceOf xs (listInsert xs x i)

{- Exercise 3
To specify the prime factorization function below, either
a) write one property-based test and two unit tests
OR
b) write one property-based test and one unit test, and change the input type

For this exercise only, you may import and use `product` from Data.List.
-}
primeFactors :: Int -> [Int]
primeFactors x = undefined

prop_factors_multiply_to_number :: Int -> Bool
prop_factors_multiply_to_number x = product (primeFactors x) == x

test_first_ten_numbers :: Test
test_first_ten_numbers =
  "first ten"
    ~: [ "one" ~: [1] ~?= primeFactors 1,
         "two" ~: [2] ~?= primeFactors 2,
         "three" ~: [3] ~?= primeFactors 3,
         "four" ~: [2, 2] ~?= primeFactors 4,
         "five" ~: [5] ~?= primeFactors 5,
         "six" ~: [2, 3] ~?= primeFactors 6,
         "seven" ~: [7] ~?= primeFactors 7,
         "eight" ~: [2, 2, 2] ~?= primeFactors 8,
         "nine" ~: [3, 3] ~?= primeFactors 9,
         "ten" ~: [2, 5] ~?= primeFactors 10
       ]

{-
Write down the number of hours it took you to complete this homework. Please
also write one question or comment about any of the material we have covered so
far.
-}

time :: Double
time = 1.5

question :: String
question = "How do I process runtime errors in test cases? I ended up making all my functions have non-crashing behavior for incorrect inputs but I'd like to also allow runtime errors too?"