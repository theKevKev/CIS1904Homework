{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use void" #-}

module Exercises where

import Control.Applicative (Alternative (..), Applicative (liftA2))
import Data.Char (isAlpha, isDigit, isLower, isSpace, isUpper)
import System.IO.Error (catchIOError)
import Test.HUnit

{-
In this homework, we continue exploring parsing. Wherever possible, you should
reuse code from last week, especially your the monad functions for parsers.
You should copy the definitions you want to reuse in at the BOTTOM of this
file.

Recall our LuParser and LuParseError types.
-}

type LuParseError = String

newtype LuParser a = LuParser {parse :: String -> Either LuParseError (a, String)}

{-
Exercise 1

First, write a function that parses the end of the file, i.e., it should
succeed exactly when there are no more characters to parse.

Notice the type of this function. A `LuParser a` parses values of type `a`, but
here we are not parsing any values, just recognizing the end of the input. To
indicate that we do not get any meaningful value from this parser, we give it
the type `LuParser ()`, i.e., it parses the end of input as `()`.

Then, using a function from last week's homework and functions from Data.Char,
write parsers for:

1. alphabetic Unicode characters
2. ASCII digits
3. uppercase (or title-case) alphabetic Unicode characters
4. lowercase alphabetic Unicode characters
5. Unicode space characters and control characters \t, \n, \r, \f, \v

Write at least two tests per parser. Be sure to add all tests to main.
-}
eof :: LuParser ()
eof = undefined

-- | Parsers for specific sorts of characters
alpha :: LuParser Char
alpha = undefined

digit :: LuParser Char
digit = undefined

upper :: LuParser Char
upper = undefined

lower :: LuParser Char
lower = undefined

space :: LuParser Char
space = undefined

{-
Exercise 2

Write a function orP that, given two parsers, returns a parser that "ors"
together the two. That is, the output parser will, on a given input string,
run the first input parser; if that fails, it will run the second.
Hint: you do not need any Monad, Applicative, or Functor functions for this
problem.

Next, write a function between that takes three parsers and returns a
parser that runs all three input parsers, keeping the results only from the
middle one.

We have written some tests for both functions. You are welcome but not
required to add more. Note that if you chose different error messages
for some of your functions, you may need to edit the tests.
-}
orP :: LuParser a -> LuParser a -> LuParser a
orP = undefined

testOrP :: Test
testOrP =
  "orP"
    ~: TestList
      [ "both" ~: parse (orP alpha (char 'x')) "xy" ~?= Right ('x', "y"),
        "first" ~: parse (orP upper digit) "X" ~?= Right ('X', ""),
        "second" ~: parse (orP lower space) " z" ~?= Right (' ', "z"),
        "neither"
          ~: parse (orP eof parseFail) "A"
          ~?= Left "This parser always fails."
      ]

betweenP :: LuParser a -> LuParser b -> LuParser c -> LuParser b
betweenP = undefined

testBetweenP :: Test
testBetweenP =
  "betweenP"
    ~: TestList
      [ "success"
          ~: parse (betweenP (char '(') alpha (char ')')) "(x)y"
          ~?= Right ('x', "y"),
        "failFirst"
          ~: parse (betweenP (char '[') digit (char ']')) "(3]"
          ~?= Left
            "Value '(' was filtered out.",
        "failMiddle"
          ~: parse (betweenP (char '{') upper (char '}')) "{x}"
          ~?= Left "Value 'x' was filtered out.",
        "failLast"
          ~: parse (betweenP (char '<') lower (char '>')) "<x"
          ~?= Left "Cannot parse empty string as character."
      ]

{-
Exercise 3

Write a function parseAll that takes a parser p and returns another parser that
succeeds only when p parses its entire input. (You should use one of the
functions you have already implemented in this.)

Next, fill in the function parseAllFromFile below that, given a parser and a
file path, reads and parses the entire contents of the file at that path,
failing if there are any contents leftover. Note that reading from a file can
result in I/O errors; we will catch these using the function catchIOError
https://hackage-content.haskell.org/package/base-4.22.0.0/docs/System-IO-Error.html#v:catchIOError
and handle them using Either.

We have written tests for both. You are welcome but not required to add more.
Note that if you chose different error messages for some of your functions,
you may need to edit the tests.
-}
parseAll :: LuParser a -> LuParser a
parseAll = undefined

testParseAll :: Test
testParseAll =
  "parseAll"
    ~: TestList
      [ "success1" ~: parse (parseAll upper) "X" ~?= Right ('X', ""),
        "success2" ~: parse (parseAll eof) "" ~?= Right ((), ""),
        "failure1"
          ~: parse (parseAll upper) "XY"
          ~?= Left "Cannot parse nonempty string Y as EOF.",
        "failure2"
          ~: parse (parseAll upper) "xY"
          ~?= Left "Value 'x' was filtered out."
      ]

parseAllFromFile :: LuParser a -> String -> IO (Either LuParseError a)
parseAllFromFile p filename = catchIOError (action p) handler
  where
    -- read from file and parse
    action :: LuParser b -> IO (Either LuParseError b)
    action = undefined

    -- handle I/O exceptions
    handler :: IOError -> IO (Either LuParseError a)
    handler = undefined

testParseAllFromFile :: IO Test
testParseAllFromFile = do
  notFound <- parseAllFromFile eof "foo"
  noParse <- parseAllFromFile (char 'c') "testFiles/x.txt"
  notAll <- parseAllFromFile (char 'x') "testFiles/xy.txt"
  success <- parseAllFromFile (char 'x') "testFiles/x.txt"
  return $
    TestList
      [ "notFound"
          ~: notFound
          ~?= Left "foo: openFile: does not exist (No such file or directory)",
        "noParse"
          ~: noParse
          ~?= Left "Value 'x' was filtered out.",
        "notAll"
          ~: notAll
          ~?= Left "Cannot parse nonempty string y as EOF.",
        "success" ~: success ~?= Right 'x'
      ]

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
  tests <- testParseAllFromFile
  _ <-
    runTestTT $
      TestList
        [ testOrP,
          testBetweenP,
          testParseAll,
          tests,
          check
        ]
  return ()

------------------------------------------------------------------------

-- COPY CODE FROM LAST WEEK HERE

char :: Char -> LuParser Char
char = undefined -- referenced in tests, replace with your definition

------------------------------------------------------------------------

-- parser for testing
parseFail :: LuParser a
parseFail = LuParser $ const $ Left "This parser always fails."