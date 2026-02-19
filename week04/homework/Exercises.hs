module Exercises where

{- Don't change the imports. -}

import Test.HUnit
import Prelude hiding (concat, concatMap)
import Test.QuickCheck

-- Exercise 1:

{-
Now we will work with a Value type that includes both IntVal and BoolVal, as
well as a NilVal and StringVal.
-}

data Value
  = NilVal
  | IntVal Int
  | BoolVal Bool
  | StringVal String
  deriving (Eq, Show, Ord)

data Uop
  = Neg -- `-` conceptually, integer negation
  | Not -- conceptually, boolean negation
  | Len -- conceptually, string length, integer value, or 1/0 for True/False
  deriving (Eq, Show)

data Bop
  = Plus -- `+` conceptually, integer addition
  | Minus -- `-` conceptually, integer subtraction
  | Times -- `*` conceptually, integer multiplication
  | Divide -- `//` conceptually, integer division
  | Modulo -- `%` conceptually, integer modulo
  | Eq -- conceptually, equality of Values
  | Gt -- conceptually, >= on any two Values
  | Ge -- conceptually, > on any two Values
  | Lt -- conceptually, < on any two Values
  | Le -- conceptually, <= on any two Values
  | Concat -- conceptually, String concat
  deriving (Eq, Show)

{-
Now we want to use Uop, Bop, and Value to handle operations that may have
IntVals, BoolVals, or StringVals. This time, for unsupported operations, we
return NilVal instead of errors.

First, fill in test cases in `exercise1`. Write at least one property based
test for practice (e.g., check that a standard mathematical relationship is
respected by Lu arithmatic). Your unit and property-based tests together should
include all of the Lu operators; for this homework, you do not need to test
every combination of operators and constructors, just at least one test per
operator.

Your week03 homework can help you with some of the Uop and Bop constructors.
However, keep in mind that now you will have to handle IntVal and BoolVal.

Notes:
- For Eq, Gt, Ge, Lt, and Le, you should use the symbols =, >, >=, <, and <=
  directly to compare _any_ two Values, not just those with the same
  constructor. (Those symbols will work on Values; more on this in our
  typeclasses unit.)
- The semantics of Len in Lu as defined in its manual* are that for a
  StringVal, it measures string length; for a BoolVal b, it is IntVal 1 if b is
  True and 0 otherwise, and for an IntVal or NilVal it is the identity
  function.
- You may use the partial functions `div` and `mod` for division, but your
  function should never crash; it should return NilVal for invalid division
  or invalid use of modulo.
-}

evalUop :: Uop -> Value -> Value
evalUop = undefined

evalBop :: Bop -> Value -> Value -> Value
evalBop = undefined

exercise1 :: Test
exercise1 =
  TestList
    [] -- add tests for each of the implemented operator functions

-- Exercise 2:
{-
We can combine `Uop` and `Bop` operations with `Value`s in a new data type:
an `Expression`.
-}

data Expression
  = Val Value -- literal Values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  deriving (Eq, Show)

{-
We want to be able to evaluate expressions using the functions you just wrote.

First fill in test cases to familiarize yourself with the Expression data type.
Include at least one property based test for practice.
Examples of Expressions:
- Val (IntVal 1)
- Op1 Neg (Val (IntVal 1))
- Op2 (Val (IntVal 1)) Plus (Val (IntVal 1))
- Op2 (Op1 Neg (Val (IntVal1))) Plus (Val (IntVal 1))
- Op2 Eq (Val (IntVal 1)) (Val (IntVal 1))
- Op1 Not (Op2 Eq (Val (IntVal 1)) (Val (IntVal 1)))

Then, fill in the eval function.
-}

eval :: Expression -> Value
eval = undefined

exercise2 :: Test
exercise2 =
  "eval"
    ~: TestList [] -- tests for eval

-- Exercise 3:
{-
For this exercise, you should not use any direct recursion or use any
recursive list functions besides `map`, `filter`, and `foldr`.

Include at least one unit test for each of `intToValue` and `sumLu` and at
least one property-based test for `sumLu`. You may use `sum` for testing ONLY.
-}

{-
Convert a list of integers to a list of the equivalent IntVals.
Hint: you can use constructors like functions here, so your solution should be
eta reduced (i.e., you should not need to directly reference the input list or
make any anonymous or helper functions).
-}
intToValue :: [Int] -> [Value]
intToValue = undefined

{-
Produce an IntVal that is the sum of all of the IntVals in a list of Values.
Note: you should do the arithmetic "in Lu", i.e., you should construct a Lu
Expression and then evaluate it.
-}
sumLu :: [Value] -> Value
sumLu = undefined

exercise3 :: Test
exercise3 = undefined

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question or comment you have about any of the material we have
covered so far, not necessarily from this week.
(Make sure you provide a Double and a String, rather than just replacing
the string after `error` with another string and leaving the `error`.)
-}

time :: Double
time = error "unimplemented"

question :: String
question = error "unimplemented"

check :: Test
check =
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
          check
        ]
  return ()

{- Please make sure to submit only this file to Gradescope. -}

{- Credit: this homework is adapted from material created by Stephanie Weirich. -}

{- *Lu Manual: https://www.seas.upenn.edu/~cis5520/current/hw/hw05/LuManual.html; note that
  we are using a simplified version of this-}