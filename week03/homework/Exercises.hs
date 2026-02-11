{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Exercises where

import Data.Int (Int)
import Test.HUnit
  ( Test (..),
    assertBool,
    runTestTT,
    (~:),
    (~?=),
  )
import Test.QuickCheck

{-
In this homework and throughout the semester we will be working with
the Lu language. The Lu language was developed by Stephanie Weirich to be a
simplified version of Lua (https://www.lua.org/manual/5.4/), a lightweight
language popularly used in video game development.

Lu Language Reference Manual:
https://www.cis.upenn.edu/~cis5520/current/hw/hw05/LuManual.html

Here is an example of the Lu langage:

x = 1 + 2 - 3 + (1 + 3)
y = 0
while x > 0 do
  y = y + x
  x = x - 1
end

To do this, we create what is known as a _deep embedding_ of Lu in Haskell.
That is, we are using Haskell to define data structures representing the
various elements of Lu syntax, which will eventually allow us to manipulate
Lu programs in Haskell.
-}

-- Exercise 1:
{-
In Lu we represent all types with the data type Value.
A Value can be a NilVal, an IntVal containing an Int, a BoolVal containing a
Bool, or a StringVal containing a String.

data Value
  = NilVal
  | IntVal Int
  | BoolVal Bool
  | StringVal String
  deriving (Eq, Show)

For this exercise, we have simplified Value to have just the IntVal constructor.
-}

data Value = IntVal Int deriving (Eq, Show)

{-
First try constructing an IntVal.
(You do not need to do so here; try writing :t <your IntVal here> in GHCi;
if the result is <your IntVal here> :: Value, you have constructed an IntVal.)
Afterwards, fill in `mkIntVal`, which makes a given `Int` an `IntVal`.

Remember: try to write idiomatic Haskell, avoiding repetition.
-}

mkIntVal :: Int -> Value
mkIntVal = undefined

-- Fill in the definition for the plus function, which adds two Values
--  pointwise.
plus :: Value -> Value -> Value
plus = undefined

-- Write unit tests for plus and mkIntVal
-- (You can optionally write property-based tests also,
--  but they are not required.)
exercise1 :: Test
exercise1 =
  TestList
    [ "plus"
        ~: TestList
          [], -- insert tests for plus here
      "mkIntVal"
        ~: TestList
          [] -- insert tests for mkIntVal here
    ]

-- Exercise 2:

{-
Now that we have Values and we can define Haskell operations on them, let's
come up with a datatype for Lu operations.

Lu has unary operators (`Uop`), which are single argument functions, and
binary operations (`Bop`), which are two argument functions.
-}

data Uop
  = Neg -- Conceptually, think of this as integer negation,
  --      taking some IntVal x to IntVal (-x)
  deriving (Eq, Show)

data Bop
  = Plus -- Conceptually, addition on Values
  | Minus -- Conceptually, subtraction on Values
  | Times -- Conceptually, multiplication on Values
  | Divide -- Conceptually, integer division on Values
  | Modulo -- Conceptually, modulo on Values
  deriving (Eq, Show)

{-
First, fill in the test cases in exercise2.
Again, you can optionally write property-based tests, but only
unit tests are required. Your unit tests should cover all operators.

Afterwards, implement evalUop and evalBop.
evalUop should take a Lu Uop and Value and return the result of applying that
operator to that value --- e.g., evalOp Neg (IntVal 3) should be IntVal (-3).
evalBop should do the same, i.e., evalOp Plus (IntVal 2) (IntvVal 3) should be
(IntVal 5).

In general, you should not use partial functions (i.e., those that crash on some
inputs) in this class,
but for this exercise, you may use the partial functions div and mod.
(https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Prelude.html#v:div)
(https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Prelude.html#v:mod).

For division, it is ok for division by 0 to crash (you do not need to test this
case), and your answer should round toward negative infinity (i.e.,
Divide (IntVal (-3)) (IntVal 2) should be -2).

For modulo, it is also ok to crash when the second argument is 0 (you do not need
to test this case), and the result should always be nonnegative.
-}

evalUop :: Uop -> Value -> Value
evalUop = undefined

evalBop :: Bop -> Value -> Value -> Value
evalBop = undefined

exercise2 :: Test
exercise2 =
  TestList
    [] -- add a TestList for each of the implemented operator functions

-- Exercise 3:
{-
Note: This exercise is optional. It is good practice, and we will give you
feedback on it if you complete it, but it will not affect your grade.
In particular, if at this point you have spent 3 or more hours on this
homework, please a) come ask any questions you have in office hours and
b) do not feel obligated to complete this problem.

Regardless of whether you do this problem, please fill in the time this
assignment took you, including exercise 3 if you complete it, as well
as a question or comment about the course material.
-}

{-
Now we want to focus on Boolean values. To avoid confusion,
let's work with a new Value ADT, a new Bop ADT, and a new Uop ADT.
-}

data Value' = BoolVal Bool deriving (Eq, Show) -- BoolVal is a boolean

data Uop'
  = Not -- Conceptually, Boolean negation of the provided argument.
  deriving (Eq, Show)

data Bop'
  = Eq -- Conceptually, checks equality of two Values
  | Ne -- -- Conceptually, checks inequality of two Values
  deriving (Eq, Show)

{-
First, fill in the test cases in exercise3.

Afterwards, implement each of the functions that correspond to the operations
in Uop' and Bop'.
-}

evalUop' :: Uop' -> Value' -> Value' -> Value'
evalUop' = undefined

evalBop' :: Bop' -> Value' -> Value' -> Value'
evalBop' = undefined

exercise3 :: Test
exercise3 =
  TestList
    [] -- add a TestList for each of the implemented operator functions

{-
Write down the number of hours it took you to complete this homework. Please
also write one question or comment you have about any of the material we have
covered so far, not necessarily from this week.
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