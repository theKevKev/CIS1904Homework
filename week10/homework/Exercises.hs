{-# LANGUAGE InstanceSigs #-}

{- HLINT ignore "Use lambda-case" -}

module Exercises where

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Data.Char
import Data.List (stripPrefix)
import Data.Map (Map, fromList, toList)
import Test.HUnit

{-
Over the next few homeworks, we will continue exploring parsing.

Recall our LuParser and LuParseError types.
-}

type LuParseError = String

newtype LuParser a = LuParser {parse :: String -> Either LuParseError (a, String)}

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

{-
First, copy in your Functor and Applicative instances for LuParser from
previous homeworks. Then, write a Monad instance for LuParser.

(As an aside, we could actually do nearly all the parsing we want to
do in this class using just Applicative. In practice, however, you will see
monads used in Haskell much more often than you will see Applicative, even when
Applicative would suffice, so we are going to use monadic operators here.)

Recall that monads should satisfy the following laws:

return a >>= k = k a (left identity)

m >>= return = m (right identity)

m >>= (\x -> k x >>= h) = (m >>= k) >>= h (associativity)

pure = return

m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

You may, if you choose, use fmap, pure, and <*> in your Monad instance
definitions. If you are stuck on one of the definitions, remember to follow
the types. Look at what type you need, and consider the different ways you
have available of constructing something of that type.
-}

instance Monad LuParser where
  return :: a -> LuParser a
  return = pure

  (>>=) :: LuParser a -> (a -> LuParser b) -> LuParser b
  p >>= func = LuParser $ \s -> case parse p s of
    Left err_str -> Left err_str
    Right (result, rem_string) -> parse (func result) rem_string

{-
As we discussed last week, testing equality of parsers (and of functions
generally) is difficult. Like last week, we will get around this by testing
that the output of two parsers is equal for a given input string. Write
one test for return and four tests for bind. (Because of the complexities of
generating functions, we have not provided a generator for parsers, but you may
if you like include property-based tests that do not take parsers as inputs.)

Be sure to add your unit tests to main.
-}

testMonad :: Test
testMonad =
  "monad implementation"
    ~: TestList
      [ "return" ~: parse (return 42) "hello" ~?= Right (42, "hello"),
        "bind 1" ~: parse (parseVar >>= \x -> return (x * 2)) "x" ~?= Right (-8, ""),
        "bind 2" ~: parse (parseFail >>= \x -> return (x + 1)) "abc" ~?= Left "This parser always fails.",
        "bind 3" ~: parse (parseVar >>= \x -> return (x + 1)) "y" ~?= Right (13, ""),
        "bind 4" ~: parse (parseVar >>= parseRevDivAux varMap) "xy" ~?= Right (-3, "")
      ]

{-
Write a function that adds a filter to a parser. That is, the resulting parser
should return the same results as the input parser if those results satisfy
the input predicate, and otherwise it should fail with an error message of your
choice. You may use do notation or use >>= directly, but you must use the
monadic functions in some form. (Hint: the simplest way to do this uses return
and bind as defined for Either, not for LuParser.
See Hoogle for details on that definition:
https://hackage-content.haskell.org/package/ghc-internal-9.1401.0/docs/src/GHC.Internal.Data.Either.html#line-150)

Include at least two tests. (Be sure to add all unit tests to main.)
-}

filterParse :: (Show a) => (a -> Bool) -> LuParser a -> LuParser a
filterParse func p = LuParser $ \s -> do
  (out, rem) <- parse p s
  if func out
    then return (out, rem)
    else Left ("Predicate failed on: " ++ show out)

testFilterParse :: Test
testFilterParse =
  "testFilterParse"
    ~: TestList
      [ "no input" ~: parse (filterParse even parseVar) "" ~?= Left "No value found for key \"\".",
        "filter even" ~: parse (filterParse even parseVar) "x" ~?= Right (-4, ""),
        "filter odd" ~: parse (filterParse (\x -> x `mod` 6 == 0) parseVar) "x" ~?= Left "Predicate failed on: -4"
      ]

{-
Write a function that, given a predicate over chars, parses any input char that
satisfies the predicate, as itself. If the input string is empty, the parser
should give a helpful error message.

(Remember, because we parse one piece of the input string at a time, there may
be more unparsed string left over after parsing the char. That is not an
error.)

Include at least three tests. (Be sure to add all unit tests to main.)
-}
charWhere :: (Char -> Bool) -> LuParser Char
charWhere func = LuParser $ \s -> case s of
  [] -> Left "No characters left."
  (x : xs) -> if func x then Right (x, xs) else Left ("Predicate failed on: " ++ show x)

testCharWhere :: Test
testCharWhere =
  "testCharWhere"
    ~: TestList
      [ "empty string" ~: parse (charWhere isAlpha) "" ~?= Left "No characters left.",
        "match" ~: parse (charWhere isAlpha) "abc" ~?= Right ('a', "bc"),
        "no match" ~: parse (charWhere isAlpha) "1bc" ~?= Left "Predicate failed on: '1'",
        "digit match" ~: parse (charWhere isDigit) "3ab" ~?= Right ('3', "ab")
      ]

{-
Write a function that parses exactly the input char, as itself.

Include at least two tests. (Be sure to add all unit tests to main.)
-}
char :: Char -> LuParser Char
char c = charWhere (== c)

testChar :: Test
testChar =
  "testChar"
    ~: TestList
      [ "match" ~: parse (char 'a') "abc" ~?= Right ('a', "bc"),
        "no match" ~: parse (char 'z') "abc" ~?= Left "Predicate failed on: 'a'"
      ]

{-
Write a function that parses exactly the input string, as itself.
(If stuck, remember what tool we have for iteratively operating on and
combining elements of a list.)

Remember to use the monadic operators, as applicable.
Hint: it also may be helpful to remember that every monad is an instance of
Functor and Applicative, too.

Include at least four tests. (Be sure to add all unit tests to main.)
-}
string :: String -> LuParser String
string = mapM char

testString :: Test
testString =
  "testString"
    ~: TestList
      [ "empty" ~: parse (string "") "abc" ~?= Right ("", "abc"),
        "full match" ~: parse (string "abc") "abc" ~?= Right ("abc", ""),
        "prefix match" ~: parse (string "ab") "abc" ~?= Right ("ab", "c"),
        "no match" ~: parse (string "xyz") "abc" ~?= Left "Predicate failed on: 'a'"
      ]

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.

Please submit ONLY this file to Gradescope.
-}

time :: Double
time = 1.5

question :: String
question = "I was really confused about whether I did char and string correctly, like my best guess was that it's creating a parser that expects a certain input, just because my first guess would have been equivalent to just the pure/return function. Basically my question is: What's the point of making this char parser or string parser? When would you want your parser to fail upon not seeing a prespecified string rather than having a real parser that reads the input? Like if you already have the prespecified string then are you just parsing to see if there's a match? "

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
        [ check,
          testMonad,
          testFilterParse,
          testCharWhere,
          testChar,
          testString
        ]
  return ()

{-
You are welcome to use the following definitions for testing.
-}

varMap :: Map String Int
varMap = fromList [("x", -4), ("y", 12), ("z", 0)]

prefixMatches :: String -> Map String a -> [Either String (a, String)]
prefixMatches s m = filter isRight (Prelude.map (tryParse s) (toList m))
  where
    tryParse :: String -> (String, a) -> Either String (a, String)
    tryParse s (k, v) = case stripPrefix k s of
      Just s' -> Right (v, s')
      Nothing -> Left (show k ++ " is not a prefix of " ++ show s ++ ".")

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

parseWithMap :: Map String a -> LuParser a
parseWithMap m = LuParser $ \s -> case prefixMatches s m of
  x : xs -> x
  _ -> Left $ "No value found for key " ++ show s ++ "."

parseVar :: LuParser Int
parseVar = parseWithMap varMap

parseRevDivAux :: Map String Int -> Int -> LuParser Int
parseRevDivAux m x = LuParser $ \s -> case (x, prefixMatches s m) of
  (0, _) -> Left "Cannot divide by zero."
  (_, Right (y, s') : ys) -> Right (y `div` x, s')
  _ -> Left $ "No value found for key " ++ show s ++ "."

-- parseRevDiv :: LuParser Int
-- parseRevDiv = parseVar >>= parseRevDivAux varMap

parseFail :: LuParser a
parseFail = LuParser $ \s -> Left "This parser always fails."