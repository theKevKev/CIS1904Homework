{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Exercises where

import Control.Applicative (liftA2)
import Data.Char
import System.IO.Error (catchIOError)
import Test.HUnit
import Text.Read (readMaybe)

{-
Now we will parse Lu statements. Note that, due to the length of this
class, there are many parsing considerations we will not be able to get to,
such as operator precedence, which would allow us to be more relaxed about
parentheses. For this homework, we will assume all Lu programs follow the
conventions described in our Show instances in Homework 6. For example,
unary operations appear as the corresponding operation symbol from Homework 6
followed by the operand in parentheses, with no space. Binary operations
appear as the first operand in parentheses, a space, the operator, another
space, and then the second operand in parentheses.

Again, wherever possible, you should reuse code from previous weeks.
You should copy this code in at the BOTTOM of this file.

Lu datatypes, as well as LuParseError and LuParser definitions, are also
included at the bottom of this file.
-}

{-
Exercise 1

Write a function `keywordP` that, given a string s and some term x of type a,
returns a parser for a keyword written as s that corresponds to x. That is, the
resulting parser should succeed exactly on the string s and no others, and it
should produce x.

Then, write a function `parensP` that, given a parser p, matches any string
consisting of parentheses around a string matched by p, discarding the
parentheses.

Write at least two tests for each and add them to `main`.
-}
keywordP :: String -> a -> LuParser a
keywordP = undefined

parens :: LuParser a -> LuParser a
parens = undefined

{-
Exercise 2

First, fill in the function `listP` that, given a parser p, returns a parser
that matches the longest possible concatenated sequence of strings matched by
p, as a list. For example, `parse (listP (char 'c')) "ccccx"` should return
`Right ("cccc", "x")`. (Note: because it matches the longest possible sequence,
this function can be subtly nonterminating in some situations. You are unlikely
to run into this behavior in this homework, but it's good to be aware of it.)

Write at least three tests for `listP` (we recommend, but do not require, more)
and add them to `main`.

Fill in the below parser for Values.
We have provided a simplified parser for Ints (leveraging Haskell's standard
library Int parser) at the bottom of the file.

Write at least two tests per constructor for `valueP` and add them to `main`.
-}

listP :: LuParser a -> LuParser [a]
listP = undefined
  where
    -- parse a nonempty list of elements matched by p
    neListP :: LuParser a -> LuParser [a]
    neListP = undefined

valueP :: LuParser Value
valueP = undefined
  where
    intValP :: LuParser Value
    intValP = undefined

    boolValP :: LuParser Value
    boolValP = undefined

    nilValP :: LuParser Value
    nilValP = undefined

    stringValP :: LuParser Value
    stringValP = undefined

{-
Exercise 3

Write parsers for Uops, Bops, and Expressions.

Write at least two tests per function in this exercise. (We highly recommend
you write more than that, but two is the minimum.) Add all tests to `main`.
-}

expP :: LuParser Expression
expP = undefined

uopP :: LuParser Uop
uopP = undefined

bopP :: LuParser Bop
bopP = undefined

{-
Exercise 4

Write a parser for Statements and at least two tests for it. (Again, we
recommend writing more, but two is the minimum.) Add the tests to `main`.

Finally, write a function that, given a path to a file containing a Lu
Statement (and no other text), parses the text at that file into a Lu
Statement. Write at least one succeeding and one failing test for this
function, and add both to `main`. (Note that for the succeeding test
you will need to provide a file with a Lu Statement.)
-}

statementP :: LuParser Statement
statementP = undefined

parseLuFile :: String -> IO (Either LuParseError Statement)
parseLuFile = undefined

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = undefined

question :: String
question = undefined

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
        [ check
        ]
  return ()

--------------------------

{-

COPY CODE FROM PREVIOUS WEEKS HERE

Definitions needed for intP have been stubbed.

-}

instance Functor LuParser

instance Applicative LuParser

instance Monad LuParser

char = undefined

digit = undefined

orP = undefined

-- int parser

intP :: LuParser Int
intP = LuParser $ \s -> case parse (digits `orP` negIntP) s of
  Right (numStr, s') -> case readMaybe numStr of
    Just x -> Right (x, s')
    _ -> Left $ "Haskell read error: Cannot convert " ++ numStr ++ " to Int."
  Left e -> Left e
  where
    negIntP = do
      sign <- char '-'
      ds <- digits
      return $ sign : ds
    digits = do
      d <- digit
      ds <- listP digit
      return $ d : ds

--------------------------

-- LuParseError, LuParaser, and Lu datatypes

type LuParseError = String

newtype LuParser a = LuParser {parse :: String -> Either LuParseError (a, String)}

data Value
  = NilVal
  | IntVal Int
  | BoolVal Bool
  | StringVal String
  deriving (Eq, Ord, Show)

data Uop
  = Neg
  | Not
  | Len
  deriving (Eq, Show)

data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Gt
  | Ge
  | Lt
  | Le
  | Concat
  deriving (Eq, Show)

data Expression
  = Val Value
  | Op1 Uop Expression
  | Op2 Expression Bop Expression
  deriving (Eq, Show)

data Statement
  = If Expression Statement Statement
  | Return Expression
  deriving (Eq, Show)