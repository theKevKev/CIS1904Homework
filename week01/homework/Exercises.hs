module Exercises where

import Test.HUnit
  ( Test (..),
    Testable (..),
    assertBool,
    runTestTT,
    (~:),
    (~?=),
  )
import Prelude hiding (sum)

{-
Homework 1: Haskell Basics

Reminder:
You should have exactly the `homework` folder open, not a parent folder (like
`week01`) or a specific file (like `Exercises.hs`). Please make sure you are
getting type-checking feedback before you start!

Note: do your best in this class to write idiomatic Haskell,
such as by effectively using pattern matching, writing type signatures for every
top-level definition, and avoiding unnecessary repetition.
For full points, make sure you are not duplicating work already done in previous
functions (i.e., in 1-6 you will use previous functions to write later ones).
-}

{-
Exercise 0: Uncomment each definition and fix the error so that it compiles.
Do not make any changes other than adding and removing parentheses.
-}

{-
In some languages, the syntax for defining a function and its arguments looks
something like f(x, y). Not so in Haskell! Do not wrap the arguments as a whole
in parentheses, and separate them with spaces instead of commas.
-}
f :: Int -> Int -> Int
f x y = x + y

{-
However, if a particular argument involves a pattern that itself has multiple
components, such as x : xs, the compiler may have trouble determining if we mean
(f x) : xs or f (x : xs). To avoid this confusion, multi-part arguments like this
need to be wrapped in parentheses.
-}
g :: Int -> [Int] -> Int
g n [] = n
g n (x : xs) = n + x

{-
Analogous principles apply when using a function. Don't wrap the arguments as
a whole in parentheses, but do wrap individual arguments in parentheses as
needed. Add and remove parentheses below to make the result 7.
-}
result :: Int
result = g (f 1 2) ([4, 5] ++ [6, 7])

{-
Exercises 1–6: Validating Credit Card Numbers

Have you ever wondered how websites validate your credit card number when you
shop online? Most credit providers rely on a checksum formula called the Luhn
algorithm for distinguishing valid numbers from random collections of digits (or
typing mistakes).

In this problem set, you will implement the algorithm, which follows this
specification:

- Considering the digits of the card number in _reverse order_, double the value
  of every other digit. For example, `9455` becomes `[5, 5, 4, 9]` becomes
  `[5, 10, 4, 18]`.

- Add the digits of the doubled values and the undoubled digits from the
  original number. For example, `[5, 10, 4, 18]` becomes
  `5 + (1 + 0) + 4 + (1 + 8) = 19`.

- Calculate the remainder when the sum is divided by 10. For the above example,
  the remainder would be 9.

- If the result equals 0, then the number is valid.

The progression of the exercises shows a common feature of good Haskell
programming: writing smaller functions that each perform a single task and then
combining these pieces to create more complex functions.

Same as Homework 0, you can execute the tests by running

```
> stack ghci Exercises.hs

Prelude> main
```

When all tests pass, the output will be

```
Cases: 10  Tried: 10  Errors: 0  Failures: 0
```

You can also execute the tests for an individual exercise (e.g. Exercise 1) with

```
Prelude> runTestTT exercise1
-}

{-
Exercise 1:

We first need to be able to break up a number into its last digit
and the rest of the number. Fill in the functions below:

Hint: Use `mod` for the first function and `div` for the second.
For this exercise, use them as infix operators.

(You can add an argument to the left of the =.)
-}

lastDigit :: Int -> Int
lastDigit x = x `mod` 10

dropLastDigit :: Int -> Int
dropLastDigit x = x `div` 10

{-
Here, we have some tests written using Haskell's unit testing library. For
example, the first one says the result of (lastDigit 1234) should be 4. You
are encouraged (but not required) to add your own!
-}
exercise1 :: Test
exercise1 =
  test
    [ "lastDigit"
        ~: [lastDigit 1234 ~?= 4],
      "dropLastDigit"
        ~: [dropLastDigit 1234 ~?= 123]
    ]

{-
Exercise 2: Now, we can break apart a number into its digits. It is actually
easier to break a number into a list of its digits in reverse order (can you
figure out why?), so we will do it that way. Fill in the function below:

For zero or negative inputs, toRevDigits should return the empty list.

Hint: use guards.
-}

toRevDigits :: Int -> [Int]
toRevDigits x
  | x <= 0 = []
  | otherwise = lastDigit x : toRevDigits (dropLastDigit x)

exercise2 :: Test
exercise2 =
  "toRevDigits"
    ~: [ toRevDigits 1234 ~?= [4, 3, 2, 1],
         toRevDigits 0 ~?= []
       ]

{-
Exercise 3: Once we have the digits in the proper order, we need to double
every other digit from left to right, starting with the second.
Fill in the function below:

-}

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther (x : y : xs) = x : (y * 2) : doubleEveryOther xs
doubleEveryOther x = x

exercise3 :: Test
exercise3 =
  "doubleEveryOther"
    ~: [doubleEveryOther [1, 2, 3, 4] ~?= [1, 4, 3, 8]]

{-
Exercise 4: Next, we will want to sum together all the *digits*
(not the integers themselves) in a list of integers.
For example, given [10, 2, 57] we will want the sum 1 + 0 + 2 + 5 + 7.

Remember, don't repeat yourself: your solution should include a function from
the previous exercises. In particular, if you find yourself writing logic to
separate the digits of a number, look for where you've done that before.

If you use a helper function (suggested but not
required), your helper and primary function should contain no more than two
cases each. If you do not, your function should contain no more than 3 cases.

You may assume all integers in the input are nonnegative.

You may get an hlint suggestion about using foldr; please ignore that for now.
Your solution should NOT use foldr.

Fill in the function below.
-}

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x : xs) = sum (toRevDigits x) + sumDigits xs

exercise4 :: Test
exercise4 =
  "sumDigits"
    ~: [ sumDigits [5, 10, 4, 18] ~?= 5 + 1 + 0 + 4 + 1 + 8,
         sumDigits [] ~?= 0
       ]

{-
Exercise 5: We are now ready to determine whether a credit card number is valid,
based on a simplified form of the Luhn algorithm.

To do this, we:
1. separate out every digit in the card number, in reverse order
2. double every other element of that list
3. sum the *digits* of every element of that list
4. check that the last digit of the result equals 0

Fill in the function below:

Again, you should use the functions you defined in
previous exercises.
-}

validate :: Int -> Bool
validate x = lastDigit (sumDigits (doubleEveryOther (toRevDigits x))) == 0

exercise5 :: Test
exercise5 =
  "validate"
    ~: [ validate 5594589764218858 ~?= True,
         validate 1234567898765432 ~?= False
       ]

{-
Exercise 6: Write down the number of hours it took you to complete this
homework. You will not be graded on this; it is just to help us calibrate
the length.
Please also write one question or comment about any of the material
we have covered so far.
-}

time :: Double
time = 0.5

question :: String
question = "Why are functions named with `div` instead of just the regular divide symbol /?"

exercise6 :: Test
exercise6 =
  TestCase
    ( assertBool
        "fill in a time and question"
        ( time >= 0
            && question /= ""
        )
    )

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1,
          exercise2,
          exercise3,
          exercise4,
          exercise5,
          exercise6
        ]
  return ()

{-
Source:
This assignment is adapted from past offerings of this class, which in turn
adapted it from the first practicum assigned in the University of Utrecht
functional programming course taught by Doaitse Swierstra, 2008-2009.
-}