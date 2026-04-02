{-# LANGUAGE InstanceSigs #-}
{- HLINT ignore "Use lambda-case" -}

module Exercises where

import Control.Applicative (Alternative(..))
import Data.List (stripPrefix)
import Data.Map (Map, fromList, toList)
import Data.Char
import Test.HUnit
import Control.Monad (guard)

{-
Over the next few homeworks, we will continue exploring parsing.

Recall our LuParser and LuParseError types.
-}

type LuParseError = String

newtype LuParser a = LuParser {parse :: String -> Either LuParseError (a, String)}

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


{-
As we discussed last week, testing equality of parsers (and of functions
generally) is difficult. Like last week, we will get around this by testing
that the output of two parsers is equal for a given input string. Write
one test for return and four tests for bind. (Because of the complexities of
generating functions, we have not provided a generator for parsers, but you may
if you like include property-based tests that do not take parsers as inputs.)

Be sure to add your unit tests to main.
-}


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
filterParse = undefined

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
charWhere = undefined

{-
Write a function that parses exactly the input char, as itself.

Include at least two tests. (Be sure to add all unit tests to main.)
-}
char :: Char -> LuParser Char
char = undefined

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
string = undefined

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
        [ check
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