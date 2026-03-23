module Exercises where

import Data.List (intercalate)
import Data.Map (Map, empty, fromList, toList)
import Test.HUnit
import Test.QuickCheck

{-
In the first exercise, we will improve the readability of our tests by making
custom instances of Show for all of the datatypes representing our language.

Note that we have added to our language Statements, which either case on a
boolean guard or directly return an expression.
-}

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
  deriving (Eq)

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
  deriving (Eq)

data Expression
  = Val Value
  | Op1 Uop Expression
  | Op2 Expression Bop Expression
  deriving (Eq)

data Statement
  = If Expression Statement Statement -- if e then s1 else s2 end
  | Return Expression
  deriving (Eq)

{-
Delete Show from the "deriving" list for Value and write a custom instance.

We have included tests to show the intended behavior. You are welcome (but not
required) to include additional tests.
-}

instance Show Value where
  show NilVal = "nil"
  show (IntVal x) = show x
  show (BoolVal x) = show x
  show (StringVal x) = show x

testShowValue :: Test
testShowValue =
  "showValue"
    ~: TestList
      [ "NilVal" ~: show NilVal ~?= "nil",
        "IntVal" ~: show (IntVal (-4)) ~?= "-4",
        "BoolVal" ~: show (BoolVal True) ~?= "True",
        "StringVal" ~: show (StringVal "foo") ~?= "\"foo\""
      ]

{-
Delete Show from the "deriving" list for Uop and write a custom instance.

We have included tests to show the intended behavior. You are welcome (but not
required) to include additional tests.
-}

instance Show Uop where
  show Neg = "-"
  show Not = "not"
  show Len = "#"

testShowUop :: Test
testShowUop =
  "showUop"
    ~: TestList
      [ "Neg" ~: show Neg ~?= "-",
        "Not" ~: show Not ~?= "not",
        "Len" ~: show Len ~?= "#"
      ]

{-
Delete Show from the "deriving" list for Bop and write a custom instance.
(Note that we have removed Modulo from the language for this and subsequent
homeworks.)

We have included tests to show the intended behavior. You are welcome (but not
required) to include additional tests.
-}

instance Show Bop where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "//"
  show Gt = ">"
  show Ge = ">="
  show Lt = "<"
  show Le = "<="
  show Eq = "=="
  show Concat = ".."

testShowBop :: Test
testShowBop =
  "showBop"
    ~: TestList
      [ "Plus" ~: show Plus ~?= "+",
        "Minus" ~: show Minus ~?= "-",
        "Times" ~: show Times ~?= "*",
        "Divide" ~: show Divide ~?= "//",
        "Gt" ~: show Gt ~?= ">",
        "Ge" ~: show Ge ~?= ">=",
        "Lt" ~: show Lt ~?= "<",
        "Le" ~: show Le ~?= "<=",
        "Eq" ~: show Eq ~?= "==",
        "Concat" ~: show Concat ~?= ".."
      ]

{-
Delete Show from the "deriving" list for Expression and write a custom
instance.

For Values, just show the underlying Value.
For unary operations, show the operation and then the operand in
parentheses.
For binary operations, show the first operand in parentheses, a space, the
operator, another space, and then the second operand in parentheses.
Do not include any spaces except where explicitly indicated.

Include at least one unit test case per constructor and add your tests to main.

Note: this implementation has the downside that it can become cluttered with
unneccessary parentheses. AFTER you have completed the homework, if you would
like to see an implementation of pretty-printing for Lu that uses a more
full-featured typeclass and produces correspondingly more readable output,
Stephanie Weirich's implementation is here
https://www.seas.upenn.edu/~cis5520/current/hw/hw05/LuSyntax.html.
-}

instance Show Expression where
  show (Val value) = show value
  show (Op1 uop exp) = show uop ++ "(" ++ show exp ++ ")"
  show (Op2 exp1 bop exp2) = "(" ++ show exp1 ++ ") " ++ show bop ++ " (" ++ show exp2 ++ ")"

testShowExpression :: Test
testShowExpression =
  "showExpression"
    ~: TestList
      [ "Val" ~: show (Val NilVal) ~?= "nil",
        "Val" ~: show (Val (IntVal 3)) ~?= "3",
        "Op1" ~: show (Op1 Neg (Val (IntVal 5))) ~?= "-(5)",
        "Op1" ~: show (Op1 Len (Val (StringVal "test"))) ~?= "#(\"test\")",
        "Op2" ~: show (Op2 (Val (IntVal 3)) Plus (Val (IntVal 4))) ~?= "(3) + (4)",
        "Op2" ~: show (Op2 (Val (StringVal "hello ")) Concat (Val (StringVal "World"))) ~?= "(\"hello \") .. (\"World\")"
      ]

{-
Delete Show from the "deriving" list for Statement and write a custom
instance.

For conditionals, show "if ", the guard in parentheses, " then ", the first
substatement in parentheses, " else ", and finally the second substatement in
parentheses.
Do not include any spaces except those shown in the strings above.

For a Return statement, just show the contained expression.

Include at least one test per constructor and add your tests to main.
-}

instance Show Statement where
  show (If exp st1 st2) = "if (" ++ show exp ++ ") then (" ++ show st1 ++ ") else (" ++ show st2 ++ ")"
  show (Return st) = show st

testShowStatement :: Test
testShowStatement =
  "showStatement"
    ~: TestList
      [ "if then else" ~: show (If (Val (BoolVal True)) (Return (Val (IntVal 0))) (Return (Val (IntVal 1)))) ~?= "if (True) then (0) else (1)",
        "if then else" ~: show (If (Op2 (Val (IntVal 1)) Lt (Val (IntVal 3))) (Return (Val (BoolVal True))) (Return (Val (BoolVal False)))) ~?= "if ((1) < (3)) then (True) else (False)",
        "if then else" ~: show (If (Val (BoolVal False)) (Return (Val (StringVal "word"))) (Return (Val NilVal))) ~?= "if (False) then (\"word\") else (nil)",
        "return" ~: show (Return (Val (IntVal 3))) ~?= "3"
      ]

{-
In a future homework, we will use the type constructor Data.Map.Map
(https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Map-Strict.html#t:Map),
which represents a dictionary.

This type already has an instance declaration for Show. (See what it looks like
using GHCi!) We would like a different definition, but in Haskell there can
only be one instance declaration per type/typeclass combination. There are
several language extensions that would allow us to get around this, but for the
purposes of this exercise we will just make a wrapper type, LuMap.

Fill in the below instance of Show for LuMap. (You may need to incadlude conditions.)
You can use any function from Data.Map.

We have included tests to show the intended behavior. The output should consist of
curly braces surrounding a comma-separated sequence of key/value pairs, with each
pair consisting of the key, an equal sign, and then the value.

You are welcome (but not required) to include additional tests.
-- -}

newtype LuMap a b = LuMap {m :: Map a b}

instance (Show a, Show b) => Show (LuMap a b) where
  show (LuMap {m = x}) = "{" ++ intercalate "," (map as_string pairs) ++ "}"
    where
      pairs = toList x
      as_string (x, y) = show x ++ "=" ++ show y

testShowMap :: Test
testShowMap =
  "showMapValue_a"
    ~: TestList
      [ "empty" ~: show (LuMap {m = empty} :: LuMap Statement [Int]) ~?= "{}",
        "short"
          ~: show (LuMap {m = fromList [(1, "one"), (2, "two")]})
          ~?= "{1=\"one\",2=\"two\"}",
        "longer"
          ~: show
            ( LuMap
                { m =
                    fromList
                      [ ('a', 97),
                        ('b', 98),
                        ('c', 99),
                        ('d', 100),
                        ('e', 101),
                        ('f', 102),
                        ('g', 103),
                        ('h', 104)
                      ]
                }
            )
          ~?= "{'a'=97,'b'=98,'c'=99,'d'=100,'e'=101,'f'=102,'g'=103,'h'=104}"
      ]

{-
Add an evaluation function for Statements.
Your solution may reference our implementation of `eval` included at the bottom
of this file.

The semantics of Lu are that for the purposes of a conditional, an expression
that evaluates to anything except NilVal and BoolVal False is considered
"true". (Any expression that does evaluate to NilVal or BoolVal False is
considered "false".) For Return statements, just return the result of
evaluating the contained expression.

Write at least 5 tests and include them in main. -}

evalS :: Statement -> Value
evalS (If exp st1 st2) = case eval exp of
  NilVal -> evalS st2
  BoolVal False -> evalS st2
  _ -> evalS st1
evalS (Return st) = eval st

testEvalS :: Test
testEvalS =
  "evalS"
    ~: TestList
      [ "return" ~: evalS (Return (Val (IntVal 42))) ~?= IntVal 42,
        "if true" ~: evalS (If (Val (BoolVal True)) (Return (Val (IntVal 1))) (Return (Val (IntVal 2)))) ~?= IntVal 1,
        "if false" ~: evalS (If (Val (BoolVal False)) (Return (Val (StringVal "yes"))) (Return (Val (StringVal "no")))) ~?= StringVal "no",
        "if nil" ~: evalS (If (Val NilVal) (Return (Val (IntVal 1))) (Return (Val (IntVal 2)))) ~?= IntVal 2,
        "if any object" ~: evalS (If (Val (StringVal "bubbles")) (Return (Val (BoolVal True))) (Return (Val (BoolVal False)))) ~?= BoolVal True
      ]

{-
Write down the number of hours it took you to complete this homework. Please
also write one question or comment you have about any of the material we have
covered so far, not necessarily from this week.)
-}

time :: Double
time = 1.5

question :: String
question = "is there a way to pattern match on multiple equivalent conditions? For example \"case x of (NilVal or BoolVal False) -> st1\" instead of having two separate lines for that? "

check :: Test
check =
  TestCase
    ( assertBool
        "Fill in a time and question."
        ( time >= 0
            && question /= ""
        )
    )

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ -- add your tests here
          check,
          testEvalUop,
          testEvalBop,
          testShowExpression,
          testShowStatement,
          testEval,
          testShowValue,
          testShowUop,
          testShowBop,
          testShowMap,
          testEvalS
        ]
  return ()

{- Please make sure to submit only this file to Gradescope. -}

{-
Credit: this homework is adapted from material created by Stephanie Weirich.
-}

{- Provided functions -}

evalUop :: Uop -> Value -> Value
evalUop Neg (IntVal i) = IntVal (-i)
evalUop Not (BoolVal b) = BoolVal (not b)
evalUop Len (StringVal s) = IntVal (length s)
evalUop Len (BoolVal True) = IntVal 1
evalUop Len (BoolVal False) = IntVal 0
evalUop Len v = v
evalUop _ _ = NilVal

evalBop :: Bop -> Value -> Value -> Value
evalBop Plus (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
evalBop Minus (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
evalBop Times (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
evalBop Divide (IntVal i1) (IntVal i2) | i2 /= 0 = IntVal (i1 `div` i2)
evalBop Eq v1 v2 = BoolVal (v1 == v2)
evalBop Gt v1 v2 = BoolVal (v1 > v2)
evalBop Ge v1 v2 = BoolVal (v1 >= v2)
evalBop Lt v1 v2 = BoolVal (v1 < v2)
evalBop Le v1 v2 = BoolVal (v1 <= v2)
evalBop Concat (StringVal s1) (StringVal s2) = StringVal (s1 ++ s2)
evalBop _ _ _ = NilVal

eval :: Expression -> Value
eval (Val v) = v
eval (Op1 u e) = evalUop u (eval e)
eval (Op2 e1 b e2) = evalBop b (eval e1) (eval e2)

testEvalUop :: Test
testEvalUop =
  "evalUop"
    ~: TestList
      [ evalUop Neg (IntVal 1) ~?= IntVal (-1),
        evalUop Neg (IntVal 0) ~?= IntVal 0,
        evalUop Neg (IntVal (-4)) ~?= IntVal 4,
        evalUop Neg (BoolVal True) ~?= NilVal,
        evalUop Neg (StringVal "") ~?= NilVal,
        evalUop Neg NilVal ~?= NilVal,
        evalUop Not (BoolVal True) ~?= BoolVal False,
        evalUop Not (BoolVal False) ~?= BoolVal True,
        evalUop Not (IntVal 0) ~?= NilVal,
        evalUop Not (StringVal "False") ~?= NilVal,
        evalUop Not NilVal ~?= NilVal,
        evalUop Len (StringVal "abc") ~?= IntVal 3,
        evalUop Len (IntVal (-5)) ~?= IntVal (-5),
        evalUop Len (BoolVal True) ~?= IntVal 1,
        evalUop Len (BoolVal False) ~?= IntVal 0,
        evalUop Len NilVal ~?= NilVal
      ]

testEvalBop :: Test
testEvalBop =
  "evalBop"
    ~: TestList
      [ evalBop Plus (IntVal (-3)) (IntVal 1) ~?= IntVal (-2),
        evalBop Plus (IntVal 4) (IntVal 0) ~?= IntVal 4,
        evalBop Plus (BoolVal True) (IntVal 1) ~?= NilVal,
        evalBop Plus (StringVal "y") (StringVal "x") ~?= NilVal,
        evalBop Minus (IntVal (-3)) (IntVal 1) ~?= IntVal (-4),
        evalBop Minus (IntVal 0) (IntVal (-1)) ~?= IntVal 1,
        evalBop Minus (BoolVal False) (IntVal 1) ~?= NilVal,
        evalBop Minus (StringVal "y") (StringVal "x") ~?= NilVal,
        evalBop Times (IntVal (-3)) (IntVal 1) ~?= IntVal (-3),
        evalBop Times (IntVal (-3)) (IntVal 0) ~?= IntVal 0,
        evalBop Times (BoolVal False) (IntVal 1) ~?= NilVal,
        evalBop Times (StringVal "y") (StringVal "x") ~?= NilVal,
        evalBop Divide (IntVal (-3)) (IntVal 1) ~?= IntVal (-3),
        evalBop Divide (IntVal (-3)) (IntVal 2) ~?= IntVal (-2),
        evalBop Divide (IntVal 3) (IntVal (-4)) ~?= IntVal (-1),
        evalBop Divide (IntVal 3) (IntVal 2) ~?= IntVal 1,
        evalBop Divide (IntVal (-3)) (IntVal 0) ~?= NilVal,
        evalBop Divide (BoolVal False) (IntVal 1) ~?= NilVal,
        evalBop Divide (StringVal "y") (StringVal "x") ~?= NilVal,
        evalBop Eq (IntVal 0) NilVal ~?= BoolVal False,
        evalBop Eq (IntVal (-3)) (IntVal 3) ~?= BoolVal False,
        evalBop Eq (IntVal (-3)) (IntVal (-3)) ~?= BoolVal True,
        evalBop Gt (IntVal 500) (StringVal "x") ~?= BoolVal False,
        evalBop Gt (IntVal (-3)) (IntVal (-3)) ~?= BoolVal False,
        evalBop Gt (IntVal (-4)) (IntVal 3) ~?= BoolVal False,
        evalBop Gt (IntVal 1) NilVal ~?= BoolVal True,
        evalBop Ge (IntVal 0) (StringVal "x") ~?= BoolVal False,
        evalBop Ge (IntVal (-3)) (IntVal 3) ~?= BoolVal False,
        evalBop Ge (IntVal (-3)) (IntVal (-3)) ~?= BoolVal True,
        evalBop Ge (IntVal 10) NilVal ~?= BoolVal True,
        evalBop Lt (IntVal 5) (StringVal "a") ~?= BoolVal True,
        evalBop Lt (IntVal (-7)) (IntVal 0) ~?= BoolVal True,
        evalBop Lt (IntVal 4) (IntVal 4) ~?= BoolVal False,
        evalBop Lt (IntVal 8) NilVal ~?= BoolVal False,
        evalBop Le (IntVal 2) (StringVal "z") ~?= BoolVal True,
        evalBop Le (IntVal (-4)) (IntVal (-4)) ~?= BoolVal True,
        evalBop Le (IntVal (-2)) (IntVal 3) ~?= BoolVal True,
        evalBop Le (IntVal (-9)) NilVal ~?= BoolVal False,
        evalBop Concat (IntVal (-3)) (IntVal 1) ~?= NilVal,
        evalBop Concat (StringVal "a") (StringVal "bc") ~?= StringVal "abc"
      ]

testEval :: Test
testEval =
  "eval"
    ~: TestList
      [ eval (Val (IntVal 1)) ~?= IntVal 1,
        eval (Op1 Neg (Val (IntVal 2))) ~?= IntVal (-2),
        eval (Op2 (Val (IntVal 0)) Plus (Val (IntVal 2))) ~?= IntVal 2,
        eval
          ( Op1
              Not
              ( Op2
                  (Op1 Neg (Val $ IntVal 3))
                  Gt
                  (Op1 Len (Val $ BoolVal False))
              )
          )
          ~?= BoolVal True
      ]
