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
  | Gt -- conceptually, >= on any two Values // this seems flipped
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
evalUop Neg val = case val of
  IntVal x -> IntVal (-x)
  _ -> NilVal
evalUop Not val = case val of
  BoolVal x -> BoolVal (not x)
  _ -> NilVal
evalUop Len val = case val of
  IntVal x -> IntVal x
  BoolVal True -> IntVal 1
  BoolVal False -> IntVal 0
  StringVal x -> IntVal (length x)
  _ -> NilVal

evalBop :: Bop -> Value -> Value -> Value
evalBop Plus val1 val2 = case (val1, val2) of
  (IntVal x, IntVal y) -> IntVal (x + y)
  _ -> NilVal
evalBop Minus val1 val2 = case (val1, val2) of
  (IntVal x, IntVal y) -> IntVal (x - y)
  _ -> NilVal
evalBop Times val1 val2 = case (val1, val2) of
  (IntVal x, IntVal y) -> IntVal (x * y)
  _ -> NilVal
evalBop Divide val1 val2 = case (val1, val2) of
  (IntVal x, IntVal 0) -> NilVal
  (IntVal x, IntVal y) -> IntVal (x `div` y)
  _ -> NilVal
evalBop Modulo val1 val2 = case (val1, val2) of
  (IntVal x, IntVal 0) -> NilVal
  (IntVal x, IntVal y) -> IntVal (x `mod` y)
  _ -> NilVal
evalBop Eq val1 val2 = BoolVal (val1 == val2)
evalBop Gt val1 val2 = BoolVal (val1 > val2)
evalBop Ge val1 val2 = BoolVal (val1 >= val2)
evalBop Lt val1 val2 = BoolVal (val1 < val2)
evalBop Le val1 val2 = BoolVal (val1 <= val2)
evalBop Concat val1 val2 = case (val1, val2) of
  (StringVal x, StringVal y) -> StringVal (x ++ y)
  _ -> NilVal

prop_test_mult_div :: Int -> Int -> Property
prop_test_mult_div x y = y /= 0 ==> evalBop Divide (evalBop Times (IntVal x) (IntVal y)) (IntVal y) == IntVal x

exercise1 :: Test
exercise1 =
  TestList
    ["Uop" ~: [
      "neg" ~: [
        "pos to neg" ~: evalUop Neg (IntVal 3) ~?= IntVal (-3),
        "neg to pos" ~: evalUop Neg (IntVal (-2)) ~?= IntVal 2,
        "error" ~: evalUop Neg (StringVal "hello world") ~?= NilVal
      ],
      "not" ~: [
        "not true" ~: evalUop Not (BoolVal True) ~?= BoolVal False,
        "not false" ~: evalUop Not (BoolVal False) ~?= BoolVal True,
        "error" ~: evalUop Not (IntVal 3) ~?= NilVal
      ],
      "len" ~: [
        "str len" ~: evalUop Len (StringVal "hello") ~?= IntVal 5,
        "int size" ~: evalUop Len (IntVal 5) ~?= IntVal 5,
        "bool cast" ~: evalUop Len (BoolVal True) ~?= IntVal 1
      ]
    ],
    "Bop" ~: [
      "plus" ~: [
        "pos plus pos" ~: evalBop Plus (IntVal 3) (IntVal 2) ~?= IntVal 5,
        "pos plus neg" ~: evalBop Plus (IntVal 3) (IntVal (-1)) ~?= IntVal 2,
        "error" ~: evalBop Plus (StringVal "hi") (IntVal 1) ~?= NilVal
      ],
      "minus" ~: [
        "pos minus pos" ~: evalBop Minus (IntVal 5) (IntVal 2) ~?= IntVal 3,
        "result neg" ~: evalBop Minus (IntVal 2) (IntVal 5) ~?= IntVal (-3),
        "error" ~: evalBop Minus (BoolVal True) (IntVal 1) ~?= NilVal
      ],
      "times" ~: [
        "pos times pos" ~: evalBop Times (IntVal 3) (IntVal 4) ~?= IntVal 12,
        "times neg" ~: evalBop Times (IntVal 3) (IntVal (-2)) ~?= IntVal (-6),
        "error" ~: evalBop Times (StringVal "hi") (IntVal 2) ~?= NilVal
      ],
      "divide" ~: [
        "pos divide pos" ~: evalBop Divide (IntVal 10) (IntVal 3) ~?= IntVal 3,
        "divide by zero" ~: evalBop Divide (IntVal 5) (IntVal 0) ~?= NilVal,
        "error" ~: evalBop Divide (BoolVal True) (IntVal 2) ~?= NilVal
      ],
      "modulo" ~: [
        "pos mod pos" ~: evalBop Modulo (IntVal 10) (IntVal 3) ~?= IntVal 1,
        "mod by zero" ~: evalBop Modulo (IntVal 5) (IntVal 0) ~?= NilVal,
        "error" ~: evalBop Modulo (StringVal "hi") (IntVal 3) ~?= NilVal
      ],
      "eq" ~: [
        "equal ints" ~: evalBop Eq (IntVal 3) (IntVal 3) ~?= BoolVal True,
        "unequal strings" ~: evalBop Eq (StringVal "bluefin tuna") (StringVal "bluefint una") ~?= BoolVal False,
        "equal bools" ~: evalBop Eq (BoolVal True) (BoolVal True) ~?= BoolVal True
      ],
      "gt" ~: [
        "greater" ~: evalBop Gt (IntVal 5) (IntVal 3) ~?= BoolVal True,
        "equal" ~: evalBop Gt (IntVal 3) (IntVal 3) ~?= BoolVal False,
        "less" ~: evalBop Gt (IntVal 2) (IntVal 5) ~?= BoolVal False,
        "greater string" ~: evalBop Gt (StringVal "tatb") (StringVal "tata") ~?= BoolVal True
      ],
      "ge" ~: [
        "greater" ~: evalBop Ge (IntVal 5) (IntVal 3) ~?= BoolVal True,
        "equal" ~: evalBop Ge (BoolVal True) (BoolVal True) ~?= BoolVal True,
        "less" ~: evalBop Ge (IntVal 2) (IntVal 5) ~?= BoolVal False
      ],
      "lt" ~: [
        "less" ~: evalBop Lt (BoolVal False) (BoolVal True) ~?= BoolVal True,
        "equal" ~: evalBop Lt (StringVal "ten trees") (StringVal "ten trees") ~?= BoolVal False,
        "different types" ~: evalBop Lt (IntVal 5) (StringVal "fourteen") ~?= BoolVal True
      ],
      "le" ~: [
        "less or equal" ~: evalBop Le (IntVal 2) (IntVal 5) ~?= BoolVal True,
        "equal" ~: evalBop Le (IntVal 3) (IntVal 3) ~?= BoolVal True,
        "diff types" ~: evalBop Le (BoolVal False) (IntVal 3) ~?= BoolVal False
      ],
      "concat" ~: [
        "two strings" ~: evalBop Concat (StringVal "hello") (StringVal " world") ~?= StringVal "hello world",
        "empty string" ~: evalBop Concat (StringVal "hi") (StringVal "") ~?= StringVal "hi",
        "error" ~: evalBop Concat (IntVal 1) (StringVal "hi") ~?= NilVal
      ]
    ]
    ] -- add tests for each of the implemented operator functions

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
eval (Val value) = value
eval (Op1 op ex) = evalUop op (eval ex)
eval (Op2 ex1 bop ex2) = evalBop bop (eval ex1) (eval ex2)

prop_neg_neg_int :: Int -> Bool
prop_neg_neg_int x = eval (Op1 Neg (Op1 Neg (Val (IntVal x)))) == IntVal x

exercise2 :: Test
exercise2 =
  "eval"
    ~: TestList
      [ "-(4+6)" ~:
          eval (Op1 Neg (Op2 (Val (IntVal 4)) Plus (Val (IntVal 6))))
            ~?= IntVal (-10),
        "len of concat" ~:
          eval (Op1 Len (Op2 (Val (StringVal "foo")) Concat (Val (StringVal "bar"))))
            ~?= IntVal 6,
        "100 - (7 * 12)" ~:
          eval (Op2 (Val (IntVal 100)) Minus (Op2 (Val (IntVal 7)) Times (Val (IntVal 12))))
            ~?= IntVal 16,
        "5 * (17 // 5) + 17 mod 5" ~:
          let v17 = Val (IntVal 17)
              v5  = Val (IntVal 5)
              divAndMult = Op2 (Op2 v17 Divide v5) Times v5
              modPart = Op2 v17 Modulo v5
          in eval (Op2 divAndMult Plus modPart)
            ~?= IntVal 17,
        "not (true == false)" ~:
          eval (Op1 Not (Op2 (Val (BoolVal True)) Eq (Val (BoolVal False))))
            ~?= BoolVal True,
        "cat < dog" ~:
          eval (Op2 (Val (StringVal "cat")) Lt (Val (StringVal "dog")))
            ~?= BoolVal True,
        "nil == (neg True)" ~:
          eval (Op2 (Val NilVal) Eq (Op1 Neg (Val (BoolVal True))))
            ~?= BoolVal True,
        "3 * 3 <= 10" ~:
          eval (Op2 (Op2 (Val (IntVal 3)) Times (Val (IntVal 3))) Le (Val (IntVal 10)))
            ~?= BoolVal True,
        "7 > 3" ~:
          eval (Op2 (Val (IntVal 7)) Gt (Val (IntVal 3)))
            ~?= BoolVal True,
        "len(False) < 1" ~:
          eval (Op2 (Op1 Len (Val (BoolVal False))) Lt (Val (IntVal 1)))
            ~?= BoolVal True,
        "5 >= 5" ~:
          eval (Op2 (Val (IntVal 5)) Ge (Val (IntVal 5)))
            ~?= BoolVal True
      ]

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
intToValue = map IntVal

{-
Produce an IntVal that is the sum of all of the IntVals in a list of Values.
Note: you should do the arithmetic "in Lu", i.e., you should construct a Lu
Expression and then evaluate it.
-}
sumLu :: [Value] -> Value
sumLu = eval . foldr (\x y -> Op2 (Val x) Plus y) (Val (IntVal 0))

prop_same_length :: [Int] -> Bool
prop_same_length xs = length xs == length (intToValue xs)

prop_sumLu_matches_sum :: [Int] -> Bool
prop_sumLu_matches_sum xs = sumLu (intToValue xs) == IntVal (sum xs)

exercise3 :: Test
exercise3 =
  TestList [
    "intToValue" ~: TestList [
      "empty" ~: intToValue [] ~?= [],
      "singleton" ~: intToValue [3] ~?= [IntVal 3],
      "many" ~: intToValue [-1, 0, 1, 2] ~?= [IntVal (-1), IntVal 0, IntVal 1, IntVal 2]
    ],
    "sumLu" ~: TestList [
      "empty" ~: sumLu [] ~?= IntVal 0,
      "singleton" ~: sumLu [IntVal 3] ~?= IntVal 3,
      "many" ~: sumLu (intToValue [-1, 0, 1, 2]) ~?= IntVal 2
    ]
  ]

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question or comment you have about any of the material we have
covered so far, not necessarily from this week.
(Make sure you provide a Double and a String, rather than just replacing
the string after `error` with another string and leaving the `error`.)
-}

time :: Double
time = 1.5

question :: String
question = "one thing I'm noticing is that forcing every function not to be partial means there's a lot of edge cases that would have to be read separately and cannot just show up via error messages. Is this a real issue for readability and debugging? "

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