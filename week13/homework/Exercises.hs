{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Exercises where

import Data.Char
-- import System.IO.Error (catchIOError)
import Test.HUnit
import Test.QuickCheck
import Text.Read (readMaybe)

{-
Now that we have a simple parser, we will write property-based tests for it.

Again, wherever possible, you should reuse code from previous weeks.
You should copy this code in at the BOTTOM of this file.

Lu datatypes, as well as LuParseError and LuParser definitions, are also
included at the bottom of this file.
-}

{-
Exercise 1

Write an Arbitrary instance for Value.
Be sure to implement both arbitrary and shrink.

Note: for simplicity, we have avoided certain String considerations such as
escaping. For this homework, you may restrict the Strings that appear as
arguments to StringVal to those containing only characters satisfying
`isAllowedChar`, provided below. Make sure to include this restriction in
shrink as well.
-}

isAllowedChar :: Char -> Bool
isAllowedChar c = isDigit c || isAsciiLower c || isAsciiUpper c

{-
Exercise 2

Write Arbitrary instances for Uop and Bop.
(Consider whether it makes sense to implement shrink here.)
-}

{-
Exercise 3
Write Arbitrary instances for Expression and Statement.
Your definition of arbitrary for each should use sized.

Be sure to implement both arbitrary and shrink.
-}

{-
Exercise 4

Write roundtrip properties to test that your parsers for values, expressions,
and statements are left inverses of your implementation of show for each.
-}

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

-- COPY CODE FROM PREVIOUS WEEKS HERE

instance Functor LuParser

instance Applicative LuParser

instance Monad LuParser

orP :: LuParser a -> LuParser a -> LuParser a
orP = undefined

char :: Char -> LuParser Char
char = undefined

digit :: LuParser Char
digit = undefined

listP :: LuParser a -> LuParser [a]
listP = undefined

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

-- Lu datatype and parsing definitions

type LuParseError = String

newtype LuParser a = LuParser {parse :: String -> Either LuParseError (a, String)}

data Value
  = NilVal
  | IntVal Int
  | BoolVal Bool
  | StringVal String
  deriving (Eq, Ord)

data Uop
  = Neg
  | Not
  | Len
  deriving (Eq, Enum, Bounded)

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
  deriving (Eq, Enum, Bounded)

data Expression
  = Val Value
  | Op1 Uop Expression
  | Op2 Expression Bop Expression
  deriving (Eq)

data Statement
  = If Expression Statement Statement
  | Return Expression
  deriving (Eq)