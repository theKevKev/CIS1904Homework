{-# LANGUAGE InstanceSigs #-}

module Solutions where

import Data.List (stripPrefix)
import Data.Map (Map, fromList, lookup, toList)
import Test.HUnit
  ( Test (TestCase, TestList),
    assertBool,
    runTestTT,
    (~:),
    (~?=),
  )

{-
This week we are going to begin exploring parsing.
-}

{-
Either a b is a type that contains either an a or a b. It is often used to
model errors, because it can represent either an error message or a result.
(By convention, the first type argument is the error type.) It is defined as
follows:

data Either a b = Left a | Right b
  deriving ( Eq, Ord, Read, Show)

The standard library defines instances of Functor, Applicative, and Foldable
for Either a, with no restrictions on the type variable a.
-}

{-
Exercise 1

Implement getVal, which, given a Map, returns either the corresponding value
for the input key or a helpful error message of your choosing. (You may use a
function from Data.Map.)
Write at least 2 tests (unit or property-based, you may choose). Make sure to
run all your tests, and if you write unit tests, add them to `main`.
(We have added example Maps at the bottom of this file that you may use.)

Then implement getAndApp, which gets a function and a value from corresponding
dictionaries and, using <*>, gets the result of applying the function to the
value. If either operation fails, getAndApp should return the corresponding
error message (defaulting to the first message if both fail).
Write at least 4 tests (unit or property-based, you may choose). Make sure to
run all your tests, and if you write unit tests, add them to `main`.
-}

type LuParseError = String

type LuKey = String

getVal :: (Show a, Ord a) => a -> Map a b -> Either LuParseError b
getVal = undefined

getAndApp ::
  Map LuKey (a -> b) ->
  Map LuKey a ->
  LuKey ->
  LuKey ->
  Either LuParseError b
getAndApp = undefined

{-
Exercise 2
Below we define LuParser, a custom type for parsing a value of type a into a
String. Note that we could have equivalently written this:

type LuParser a = String -> Either LuError (a, String)

For documentation purposes/to construct an interface for our
types, we instead wrap it as shown below.

We can think of this as updating state as we parse. We take in a String, parse
a little bit of it (potentially failing), and then return both the value we
parsed and the remainder of the string that we haven't gotten to yet.

Fill in the rest of the Functor instance for LuParser below. Note that to use
parse for a LuParser p on a string s, we write `parse p s`, and that the
\$ symbol just affects precedence (i.e., you can think of it as being
similar to putting everything following it in parentheses).

Remember that Either LuError is already an instance of Functor.

Testing functions over parsers is difficult. In general, it is very
awkward both to generate functions and to test whether two functions are
equal. (Consider why this is the case.) For the purposes of this homework,
we will get around this by running our parsers.
For example, because it is hard to test if p1 is equal to p2 for parsers
p1 and p2, we will instead check if parse p1 s is equal to parse p2 s for
some String s. We have included examples below, testing the functor identity
law. (In Haskell, the composition law is actually implied by the identity law,
so we do not need to test that.)

Write at least 2 more unit tests. Make sure to run all your tests and include
them in `main`.
We have added some toy parsers at the bottom of this file for testing.
(Note that, again because function equality is complicated, you should
be careful with how you use parseFunc in tests.)
-}

newtype LuParser a = LuParser {parse :: String -> Either LuParseError (a, String)}

instance Functor LuParser where
  fmap :: (a -> b) -> LuParser a -> LuParser b
  fmap f p = LuParser $ \s -> undefined
    where
      mapFst :: (a -> b) -> (a, c) -> (b, c)
      mapFst = undefined -- map over the first element in a pair

testFmap :: Test
testFmap =
  "fmap"
    ~: TestList
      [ "idOK" ~: parse (fmap id parseVar) "x" ~?= parse parseVar "x",
        "idFail" ~: parse (fmap id parseVar) "foo" ~?= parse parseVar "foo",
        "square" ~: parse (fmap (^ 2) parseVar) "xyz" ~?= Right (16, "yz"),
        "addOne" ~: parse (fmap (+ 1) parseVar) "yz" ~?= Right (58, "z")
      ]

{-
Exercise 3
Fill in the definition of `pure` in the instance of Applicative for LuParser
below. Remember that a parser here is just an elaborate container; all `pure`
does is put a value in a container.
Write at least 1 unit test for pure. Make sure to run all your tests and add
them to `main`.

Then, fill in the undefined portions of the implementation of <*>. Recall that
we defined LuParser as a wrapper for the type

String -> Either LuParseError (a, String)

where (a, String) represents the parsed value and the remaining portion of the
input that has not yet been parsed. We can think of p1 <*> p2 as parsing a
segment of input string using p1, then parsing the next segment using p2, and
then applying the function we parsed using p1 to the value we parsed using p2.
(Right now we are not handling whitespace; we will deal with that in a future
homework.)

Conceptually, if the first parse fails, we stop computation and return an error
message (this is why `Either` is in our Parser type; your implementation should
not use Haskell's `error` function). Otherwise, we continue with the second
parse.

We have included a few tests for <*>. You are welcome (but not required) to
write more.
-}

instance Applicative LuParser where
  pure :: a -> LuParser a
  pure x = LuParser $ \s -> Right (x, s)

  (<*>) :: LuParser (a -> b) -> LuParser a -> LuParser b
  p1 <*> p2 = LuParser $ \s -> case parse p1 s of
    Left e -> undefined
    Right (f, s') -> undefined

testApply :: Test
testApply =
  "<*>"
    ~: TestList
      [ parse (parseFunc <*> parseVar) "addOnex" ~?= Right (-3, ""),
        parse (parseFail <*> parseVar) "x"
          ~?= ( Left "This parser always fails." ::
                  Either LuParseError (Bool, String)
              ),
        parse (parseFunc <*> parseVar) "negyfoobar" ~?= Right (-57, "foobar"),
        parse (parseFunc <*> parseVar) "neg"
          ~?= Left "No value found for key \"\".",
        parse (parseFail <*> parseVar) "1"
          ~?= ( Left "This parser always fails." ::
                  Either LuParseError ((), String)
              )
      ]

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.

Please submit ONLY this file to Gradescope.
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
        [ check,
          testFmap,
          testApply
        ]
  return ()

-- Example maps
funMap :: Map LuKey (Int -> Int)
funMap = fromList [("addOne", (+ 1)), ("square", (^ 2)), ("neg", negate)]

varMap :: Map LuKey Int
varMap = fromList [("x", -4), ("y", 57), ("z", 0)]

emptyMap :: Map LuKey ()
emptyMap = mempty -- this is from another typeclass, Monoid

parseWithMap :: Map LuKey a -> LuParser a
parseWithMap m = LuParser $ \s -> case successes s m of
  x : xs -> x
  _ -> Left $ "No value found for key " ++ show s ++ "."
  where
    tryParse :: String -> (LuKey, a) -> Either String (a, String)
    tryParse s (k, v) = case stripPrefix k s of
      Just s' -> Right (v, s')
      Nothing -> Left (show k ++ " is not a prefix of " ++ show s ++ ".")

    isLeft :: Either a b -> Bool
    isLeft (Left _) = True
    isLeft _ = False

    successes :: String -> Map LuKey a -> [Either String (a, String)]
    successes s m = dropWhile isLeft (map (tryParse s) (toList m))

parseFunc :: LuParser (Int -> Int)
parseFunc = parseWithMap funMap

parseVar :: LuParser Int
parseVar = parseWithMap varMap

parseFail :: LuParser a
parseFail = LuParser $ \s -> Left "This parser always fails."