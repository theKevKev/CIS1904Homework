{-# LANGUAGE BangPatterns #-}

module Exercises where

import Data.Char (toLower, toUpper)
import Test.HUnit
import Test.QuickCheck

{- In this homework, we will take a break from our ongoing Lu project to
  explore some of the implications of lazy evaluation. -}

-- Exercise 1: Fibonacci numbers

-- Exercise 1a

{-
fib n should return the nth Fibonacci number, where:
fib 0 = 0
fib 1 = 1
For n < 0, fib n = - (fib (- n)).

A simple recursive definition is fine.
Include at least 3 unit tests.
-}

fib :: Integer -> Integer
fib n
  | n < 0 = - fib (-n)
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n-1) + fib (n-2)

testFib :: Test
testFib = TestList 
  [
    "zero" ~: fib 0 ~?= 0,
    "one" ~: fib 1 ~?= 1,
    "two" ~: fib 2 ~?= 1,
    "six" ~: fib 6 ~?= 8,
    "neg six" ~: fib (-6) ~?= (-8)
  ]

{- Unfortunately, `fib` is very slow. The complexity of calculating the nth
   number is exponential in n. Below we have a tail recursive version. `go` is
   the customary keyword to use in Haskell for this sort of internally-defined
   auxiliary function. `!` here forces strict evaluation.

   You do not need to edit this definition, but write one property-based test
   for it. Hint: it should be a _model-based_ test. Note: because `fib` is
   extremely slow, this test will be extremely slow. To try it with fewer test
   cases (say, 10), instead of `quickCheck prop_FibFib'`, run
   `quickCheckWith stdArgs {maxSuccess = 10} prop_FibFib'`. -}

fib' :: Integer -> Integer
fib' n = go (0, 1) n
  where
    go (!x, !y) !n
      | n > 0 = go (y, x + y) (n - 1)
      | n < 0 = -(go (y, x + y) ((-n) - 1))
      | otherwise = x

prop_FibFib' :: Integer -> Property
prop_FibFib' n = (-32 <= n) && (n <= 32) ==> (fib n) === (fib' n) -- I added the -32 < n < 32 to increase the speed of the test
-- Exercise 1b

{- Define the infinite list of all (nonnegative) Fibonacci numbers, using fib.
  Hint: `[0..10]` in Haskell denotes the list of all integers from 0 to 10,
  inclusive. `[0..]` denotes the list of all nonnegative integers.

  You should NOT use direct recursion in this problem; combinators such as
  `map`, `filter`, and `fold` are fine.

  Include at least 1 unit test. (Hint: use `take` to test a prefix of the
  list.) -}

fibs :: [Integer]
fibs = map fib [0..]

testFibs :: Test
testFibs = TestList 
  [
    "zero" ~: take 0 fibs ~?= [], 
    "one" ~: take 1 fibs ~?= [0],
    "two" ~: take 2 fibs ~?= [0, 1], 
    "five" ~: take 5 fibs ~?= [0, 1, 1, 2, 3]
  ]

{- Unfortunately, `fibs` is also very slow. Laziness means we don't calculate
  values unless we need them, but it doesn't make the calculation any faster
  when we do need them. Implement fibs' below such that output is identical
  to fibs but computing the first n elements takes O(n) additions.
  You may use the partial function `tail` and the list function `zipWith`.

  Hint:
       fibs = [0, 1, 1, 2, ...]
  tail fibs = [1, 1, 2, 3, ...]

  Include at least one unit test. Again, use `take`.
  Note: At first glance, this looks like an excellent candidate for
  property-based testing: we have an intuitive model against which we can test
  our optimized function. In this case, however, there is only list; there is
  no input that we can randomly generate to get different test cases, so PBT is
  unnecessary here.
-}

fibs' :: [Integer]
fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')

testFibs' :: Test
testFibs' = TestList 
  [
    "zero" ~: take 0 fibs' ~?= [], 
    "one" ~: take 1 fibs' ~?= [0],
    "two" ~: take 2 fibs' ~?= [0, 1], 
    "five" ~: take 5 fibs' ~?= [0, 1, 1, 2, 3]
  ]
{- In the example above, we have to either use the partial function `tail`, or
    match on `fibs` and come up with some default behavior in the impossible
    case that `fibs` is the empty list. This is annoying; it would be nice to
    have a type that encapsulated the idea of a list being nonempty.
    In the finite case, we might use a nonempty list type (the standard library
    has one called NonEmpty). Here, we use streams to also capture within the
    type the fact that lists like `fibs` are infinite. -}

-- Exercise 2: Streams

{- We saw the below definition in class. -}
data Stream a = Cons a (Stream a)

{- We will implement a small standard library for streams.

  Some of the library functions have been implemented already; you should NOT
  use these in your implementations of the other functions, only for testing.
  The one exception is that you may use `streamGenerate` in `nats`. -}

{- Create a stream consisting of the input element repeated infinitely.
    For testing use only. -}
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

{- Make a Stream from a list. If the list is finite, all remaining elements
    have the default value. For testing use only. -}
streamFromList :: a -> [a] -> Stream a
streamFromList = foldr Cons . streamRepeat

-- Exercise 2a

{- Write a function that converts a Stream to an infinite list.

  For testing, we have provided a function `streamTake` that, given an integer
  n and a stream, returns the first n elements of the stream. We demonstrate
  its use in some unit tests below. Using `streamTake` and the list function
  `take` to truncate outputs, write two property-based tests for
  `streamToList`. Hint: this is a good situation for round-trip properties. -}

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

testStreamToList :: Test
testStreamToList =
  "streamToList"
    ~: [ "collatz7" ~: (take 3 . streamToList) collatz7 ~?= [7, 22, 11],
         "ackermann2_1" ~: (take 3 . streamToList) ackermann2_1 ~?= [1, 5, 13],
         "triangle" ~: (take 4 . streamToList) triangle ~?= [0, 1, 3, 6],
         "batteries" ~: (take 2 . streamToList) batteries ~?= ["A", "AA"]
       ]

prop_ListStreamList :: [Int] -> Property
prop_ListStreamList xs = (take (length xs) . streamToList . streamFromList 0) xs === xs

prop_StreamListStream :: Stream Int -> Property
prop_StreamListStream xs = (streamTake 100 . streamFromList 0 . streamToList) xs === streamTake 100 xs

-- Exercise 2b

{- Write map, but for Streams. Include at least 3 unit test cases.
  Again, you may use `streamTake` and the example streams `collatz7`,
  `ackermann2_1`, `triangle`, and `batteries` in your tests. -}
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap func (Cons x xs) = Cons (func x) (streamMap func xs)

testStreamMap :: Test
testStreamMap = TestList
  [
    "collatz7" ~: streamTake 5 (streamMap (2*) collatz7) ~?= [14, 44, 22, 68, 34], 
    "ackermann2_1" ~: streamTake 7 (streamMap (\x -> x - 1) ackermann2_1) ~?= [0, 4, 12, 28, 60, 124, 252],
    "batteries? nah sheep" ~: streamTake 10 (streamMap ("B"++) batteries) ~?= ["BA", "BAA", "BAAA", "BAAAA", "BAAAAA", "BAAAAAA", "BAAAAAAA", "BAAAAAAAA", "BAAAAAAAAA", "BAAAAAAAAAA"]
  ]

-- Exercise 2c

{- Given an initial element and a formula for constructing each element from
    the previous, this generates a Stream. You may use this to implement
    `nats`. -}
streamGenerate :: (a -> a) -> a -> Stream a
streamGenerate f x = Cons x (streamGenerate f (f x))

{- Create a stream of the natural numbers. (Recall that zero is a natural
    number.) Write at least one unit test. -}
nats :: Stream Integer
nats = streamGenerate (1+) 0

testNats :: Test
testNats = "first 10" ~: streamTake 10 nats ~?= [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.

Finally, test all your functions by running `main` in GHCi.
-}

time :: Double
time = 2

question :: String
question = "why does the fibs' approach work faster in O(N)? I understand why it's correct but not why it's faster, especially since we have lazy evaluation. "

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
        [ testFib,
          testFibs,
          testFibs',
          testStreamToList,
          testStreamMap,
          testNats,
          check
        ]
  return ()

{- Testing infrastructure -}

streamTake :: Int -> Stream a -> [a]
streamTake n (Cons x xs) | n > 0 = x : streamTake (n - 1) xs
streamTake _ _ = []

showStream :: (Show a) => Int -> Stream a -> String
showStream n = show . streamTake n

instance (Show a) => Show (Stream a) where
  show = showStream 5

instance (Arbitrary a) => Arbitrary (Stream a) where
  arbitrary = do
    x <- arbitrary
    streamFromList x <$> arbitrary

{- Some streams for testing -}

collatz :: Integer -> Integer
collatz n | even n = n `div` 2
collatz n = 3 * n + 1

ackermann :: Integer -> Integer -> Integer
ackermann m n | m < 0 || n < 0 = 0
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz7 :: Stream Integer
collatz7 = streamGenerate collatz 7

ackermann2_1 :: Stream Integer
ackermann2_1 = streamGenerate (ackermann 2) 1

triangle :: Stream Integer
triangle = go 0
  where
    go x = Cons ((x ^ 2 + x) `div` 2) (go (x + 1))

batteries :: Stream String
batteries = streamGenerate ('A' :) "A"

{- Tests for provided functions -}

testFib' :: Test
testFib' =
  "fib'"
    ~: [ "zero" ~: fib' 0 ~?= 0,
         "one" ~: fib' 1 ~?= 1,
         "six" ~: fib' 6 ~?= 8,
         "negFive" ~: fib' (-5) ~?= (-5)
       ]

testStreamRepeat :: Test
testStreamRepeat =
  "streamRepeat"
    ~: [ "seven" ~: streamTake 5 (streamRepeat 7) ~?= [7, 7, 7, 7, 7],
         "true" ~: streamTake 5 (streamRepeat True) ~?= [True, True, True, True, True],
         "c" ~: streamTake 5 (streamRepeat 'c') ~?= "ccccc"
       ]

testStreamFromList :: Test
testStreamFromList =
  "streamFromList"
    ~: [ "empty" ~: (streamTake 3 . streamFromList 7) [] ~?= [7, 7, 7],
         "several" ~: (streamTake 3 . streamFromList 1) [2] ~?= [2, 1, 1],
         "all" ~: (streamTake 3 . streamFromList 0) [4, 2, 6] ~?= [4, 2, 6]
       ]

testStreamGenerate :: Test
testStreamGenerate =
  "streamGenerate"
    ~: (streamTake 5 . streamGenerate (* 2)) 1
    ~?= [1, 2, 4, 8, 16]

testCollatz7 :: Test
testCollatz7 =
  "collatz7"
    ~: streamTake 17 collatz7
    ~?= [7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]

testAckermann2_1 :: Test
testAckermann2_1 =
  "ackermann2_1"
    ~: streamTake 10 ackermann2_1
    ~?= [1, 5, 13, 29, 61, 125, 253, 509, 1021, 2045]

testTriangle :: Test
testTriangle =
  "triangle"
    ~: streamTake 5 triangle
    ~?= [0, 1, 3, 6, 10]

testBatteries :: Test
testBatteries =
  "batteries"
    ~: streamTake 5 batteries
    ~?= ["A", "AA", "AAA", "AAAA", "AAAAA"]

testProvided :: IO ()
testProvided = do
  _ <-
    runTestTT $
      TestList
        [ testFib',
          testStreamToList,
          testStreamRepeat,
          testStreamFromList,
          testStreamGenerate,
          testCollatz7,
          testAckermann2_1,
          testTriangle,
          testBatteries
        ]
  return ()