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
eof = LuParser $ \s -> case s of
  [] -> Right ((), s)
  x : xs -> Left "Not yet end of file. "

-- | Parsers for specific sorts of characters
alpha :: LuParser Char
alpha = charWhere isAlpha

digit :: LuParser Char
digit = charWhere isDigit

upper :: LuParser Char
upper = charWhere isUpper

lower :: LuParser Char
lower = charWhere isLower

space :: LuParser Char
space = charWhere isSpace

testParsers :: Test
testParsers =
  "testParsers"
    ~: TestList
      [ "test eof"
          ~: TestList
            [ "eof success" ~: parse eof "" ~?= Right ((), ""),
              "eof failure" ~: parse eof "words" ~?= Left "Not yet end of file. "
            ],
        "test alpha"
          ~: TestList
            [ "alpha letter" ~: parse alpha "abc" ~?= Right ('a', "bc"),
              "alpha non-letter" ~: parse alpha "123" ~?= Left "Predicate failed on: '1'"
            ],
        "test digit"
          ~: TestList
            [ "digit number" ~: parse digit "123" ~?= Right ('1', "23"),
              "digit non-number" ~: parse digit "abc" ~?= Left "Predicate failed on: 'a'"
            ],
        "test upper"
          ~: TestList
            [ "upper uppercase" ~: parse upper "ABC" ~?= Right ('A', "BC"),
              "upper lowercase" ~: parse upper "abc" ~?= Left "Predicate failed on: 'a'"
            ],
        "test lower"
          ~: TestList
            [ "lower lowercase" ~: parse lower "abc" ~?= Right ('a', "bc"),
              "lower uppercase" ~: parse lower "ABC" ~?= Left "Predicate failed on: 'A'"
            ],
        "test space"
          ~: TestList
            [ "space space char" ~: parse space " abc" ~?= Right (' ', "abc"),
              "space tab char" ~: parse space "\tabc" ~?= Right ('\t', "abc"),
              "space tab char" ~: parse space "\nabc" ~?= Right ('\n', "abc"),
              "space tab char" ~: parse space "\rabc" ~?= Right ('\r', "abc"),
              "space tab char" ~: parse space "\fabc" ~?= Right ('\f', "abc"),
              "space tab char" ~: parse space "\vabc" ~?= Right ('\v', "abc"),
              "space non-space" ~: parse space "abc" ~?= Left "Predicate failed on: 'a'"
            ]
      ]

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
orP parser1 parser2 = LuParser $ \s -> case parse parser1 s of
  Left err_str -> parse parser2 s
  Right output -> Right output

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
betweenP p1 p2 p3 = do
  _ <- p1
  o2 <- p2
  _ <- p3
  return o2

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
            "Predicate failed on: '('",
        "failMiddle"
          ~: parse (betweenP (char '{') upper (char '}')) "{x}"
          ~?= Left "Predicate failed on: 'x'",
        "failLast"
          ~: parse (betweenP (char '<') lower (char '>')) "<x"
          ~?= Left "No characters left."
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
parseAll p = betweenP (pure ()) p eof

testParseAll :: Test
testParseAll =
  "parseAll"
    ~: TestList
      [ "success1" ~: parse (parseAll upper) "X" ~?= Right ('X', ""),
        "success2" ~: parse (parseAll eof) "" ~?= Right ((), ""),
        "failure1"
          ~: parse (parseAll upper) "XY"
          ~?= Left "Not yet end of file. ",
        "failure2"
          ~: parse (parseAll upper) "xY"
          ~?= Left "Predicate failed on: 'x'"
      ]

parseAllFromFile :: LuParser a -> String -> IO (Either LuParseError a)
parseAllFromFile p filename = catchIOError (action p) handler
  where
    -- read from file and parse
    action :: LuParser b -> IO (Either LuParseError b)
    action p =
      readFile filename >>= \s -> case parse (parseAll p) s of
        Left error -> return (Left error)
        Right (b, rem) -> return (Right b)

    -- handle I/O exceptions
    handler :: IOError -> IO (Either LuParseError a)
    handler err = return (Left (show err))

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
          ~?= Left "Predicate failed on: 'x'",
        "notAll"
          ~: notAll
          ~?= Left "Not yet end of file. ",
        "success" ~: success ~?= Right 'x'
      ]

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = 1

question :: String
question = "were monads a thing first or was imperative programming first and then people found a way to describe imperative programming in functional paradigms with monads? "

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
        [ testParsers,
          testOrP,
          testBetweenP,
          testParseAll,
          tests,
          check
        ]
  return ()

------------------------------------------------------------------------

-- COPY CODE FROM LAST WEEK HERE

instance Functor LuParser where
  fmap :: (a -> b) -> LuParser a -> LuParser b
  fmap f p = LuParser $ \s -> case parse p s of
    Left err_str -> Left err_str
    Right tup -> Right (mapFst f tup)
    where
      mapFst :: (a -> b) -> (a, c) -> (b, c)
      mapFst f (fst, snd) = (f fst, snd)

instance Applicative LuParser where
  pure :: a -> LuParser a
  pure output = LuParser $ \s -> Right (output, s)

  (<*>) :: LuParser (a -> b) -> LuParser a -> LuParser b
  p1 <*> p2 = LuParser $ \s -> case parse p1 s of
    Left e -> Left e
    Right (f, s') -> case parse p2 s' of
      Left e' -> Left e'
      Right (v, s'') -> Right (f v, s'')

instance Monad LuParser where
  return :: a -> LuParser a
  return = pure

  (>>=) :: LuParser a -> (a -> LuParser b) -> LuParser b
  p >>= func = LuParser $ \s -> case parse p s of
    Left err_str -> Left err_str
    Right (result, rem_string) -> parse (func result) rem_string

charWhere :: (Char -> Bool) -> LuParser Char
charWhere func = LuParser $ \s -> case s of
  [] -> Left "No characters left."
  (x : xs) -> if func x then Right (x, xs) else Left ("Predicate failed on: " ++ show x)

char :: Char -> LuParser Char
char c = charWhere (== c)

------------------------------------------------------------------------

-- parser for testing
parseFail :: LuParser a
parseFail = LuParser $ const $ Left "This parser always fails."