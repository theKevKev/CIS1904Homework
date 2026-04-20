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
keywordP keyword value = fmap (const value) (string keyword)

parens :: LuParser a -> LuParser a
parens p = betweenP (char '(') p (char ')')

testKeywordP :: Test
testKeywordP =
  "keywordP"
    ~: TestList
      [ "example 1" ~: parse (keywordP "hit" 3) "hittwo" ~?= Right (3, "two"),
        "example 2" ~: parse (keywordP "xyzxyz" 12) "xyzxy3" ~?= Left "Predicate failed on: '3'"
      ]

testParens :: Test
testParens =
  "testParens"
    ~: TestList
      [ "success with remainder" ~: parse (parens (char 'x')) "(x)rest" ~?= Right ('x', "rest"),
        "success exact" ~: parse (parens (string "hi")) "(hi)" ~?= Right ("hi", ""),
        "missing open paren" ~: parse (parens (char 'x')) "x)" ~?= Left "Predicate failed on: 'x'",
        "missing close paren" ~: parse (parens (char 'x')) "(x" ~?= Left "No characters left.",
        "wrong inner content" ~: parse (parens (char 'x')) "(y)" ~?= Left "Predicate failed on: 'y'"
      ]

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
listP p = orP (neListP p) (pure [])
  where
    -- parse a nonempty list of elements matched by p
    neListP :: LuParser a -> LuParser [a]
    neListP p' = do
      out <- p'
      rem <- listP p'
      return (out : rem)

testListP :: Test
testListP =
  "listP"
    ~: TestList
      [ "zero matches" ~: parse (listP (char 'c')) "xyz" ~?= Right ([], "xyz"),
        "single match" ~: parse (listP (char 'c')) "cx" ~?= Right ("c", "x"),
        "multiple matches" ~: parse (listP (char 'c')) "ccccx" ~?= Right ("cccc", "x"),
        "full consumption" ~: parse (listP (char 'c')) "ccc" ~?= Right ("ccc", ""),
        "empty input" ~: parse (listP (char 'c')) "" ~?= Right ([], "")
      ]

valueP :: LuParser Value
valueP = intValP `orP` boolValP `orP` stringValP `orP` nilValP
  where
    intValP :: LuParser Value
    intValP = fmap IntVal intP

    boolValP :: LuParser Value
    boolValP = fmap BoolVal (keywordP "True" True `orP` keywordP "False" False)

    nilValP :: LuParser Value
    nilValP = keywordP "nil" NilVal

    stringValP :: LuParser Value
    stringValP = fmap StringVal (betweenP (char '"') (listP (charWhere (/= '"'))) (char '"'))

testValueP :: Test
testValueP =
  "valueP"
    ~: TestList
      [ "int positive" ~: parse valueP "42rest" ~?= Right (IntVal 42, "rest"),
        "int negative" ~: parse valueP "-5" ~?= Right (IntVal (-5), ""),
        "bool true" ~: parse valueP "True" ~?= Right (BoolVal True, ""),
        "bool false" ~: parse valueP "Falserest" ~?= Right (BoolVal False, "rest"),
        "nil" ~: parse valueP "nil" ~?= Right (NilVal, ""),
        "nil with remainder" ~: parse valueP "nilrest" ~?= Right (NilVal, "rest"),
        "string" ~: parse valueP "\"hello\"" ~?= Right (StringVal "hello", ""),
        "string empty" ~: parse valueP "\"\"rest" ~?= Right (StringVal "", "rest")
      ]

{-
Exercise 3

Write parsers for Uops, Bops, and Expressions.

Write at least two tests per function in this exercise. (We highly recommend
you write more than that, but two is the minimum.) Add all tests to `main`.
-}

expP :: LuParser Expression
expP = valP `orP` op1P `orP` op2P
  where
    valP :: LuParser Expression
    valP = fmap Val valueP

    op1P :: LuParser Expression
    op1P = do
      uop <- uopP
      exp <- parens expP
      return (Op1 uop exp)

    op2P :: LuParser Expression
    op2P = do
      exp1 <- parens expP
      char ' '
      bop <- bopP
      char ' '
      exp2 <- parens expP
      return (Op2 exp1 bop exp2)

uopP :: LuParser Uop
uopP = negP `orP` notP `orP` lenP
  where
    negP :: LuParser Uop
    negP = keywordP "-" Neg

    notP :: LuParser Uop
    notP = keywordP "not" Not

    lenP :: LuParser Uop
    lenP = keywordP "#" Len

bopP :: LuParser Bop
bopP =
  keywordP ">=" Ge
    `orP` keywordP "<=" Le
    `orP` keywordP "==" Eq
    `orP` keywordP "//" Divide
    `orP` keywordP ".." Concat
    `orP` keywordP "+" Plus
    `orP` keywordP "-" Minus
    `orP` keywordP "*" Times
    `orP` keywordP ">" Gt
    `orP` keywordP "<" Lt

testExpP :: Test
testExpP =
  "expP"
    ~: TestList
      [ "val int" ~: parse expP "42" ~?= Right (Val (IntVal 42), ""),
        "val string" ~: parse expP "\"hello\"" ~?= Right (Val (StringVal "hello"), ""),
        "op1 neg" ~: parse expP "-(5)" ~?= Right (Op1 Neg (Val (IntVal 5)), ""),
        "op1 not" ~: parse expP "not(True)" ~?= Right (Op1 Not (Val (BoolVal True)), ""),
        "op2 plus" ~: parse expP "(3) + (4)" ~?= Right (Op2 (Val (IntVal 3)) Plus (Val (IntVal 4)), ""),
        "op2 concat" ~: parse expP "(\"hi\") .. (\"there\")" ~?= Right (Op2 (Val (StringVal "hi")) Concat (Val (StringVal "there")), ""),
        "nested op1 of op2" ~: parse expP "-((3) + (4))" ~?= Right (Op1 Neg (Op2 (Val (IntVal 3)) Plus (Val (IntVal 4))), ""),
        "nested op2 of op1" ~: parse expP "(-(3)) + (4)" ~?= Right (Op2 (Op1 Neg (Val (IntVal 3))) Plus (Val (IntVal 4)), ""),
        "roundtrip op2 nested" ~: let e = Op2 (Op1 Neg (Val (IntVal 3))) Plus (Val (IntVal 4)) in parse expP (show e) ~?= Right (e, ""),
        "roundtrip op1 of op2" ~: let e = Op1 Not (Op2 (Val (BoolVal True)) Eq (Val (BoolVal False))) in parse expP (show e) ~?= Right (e, "")
      ]

testUopP :: Test
testUopP =
  "uopP"
    ~: TestList
      [ "neg" ~: parse uopP "-" ~?= Right (Neg, ""),
        "neg with remainder" ~: parse uopP "-(x)" ~?= Right (Neg, "(x)"),
        "not" ~: parse uopP "not" ~?= Right (Not, ""),
        "not with remainder" ~: parse uopP "not(x)" ~?= Right (Not, "(x)"),
        "len" ~: parse uopP "#" ~?= Right (Len, ""),
        "len with remainder" ~: parse uopP "#(x)" ~?= Right (Len, "(x)"),
        "invalid" ~: parse uopP "+" ~?= Left "Predicate failed on: '+'"
      ]

testBopP :: Test
testBopP =
  "bopP"
    ~: TestList
      [ "plus" ~: parse bopP "+" ~?= Right (Plus, ""),
        "plus remainder" ~: parse bopP "+ " ~?= Right (Plus, " "),
        "minus" ~: parse bopP "-" ~?= Right (Minus, ""),
        "minus remainder" ~: parse bopP "- " ~?= Right (Minus, " "),
        "times" ~: parse bopP "*" ~?= Right (Times, ""),
        "times remainder" ~: parse bopP "* " ~?= Right (Times, " "),
        "divide" ~: parse bopP "//" ~?= Right (Divide, ""),
        "divide remainder" ~: parse bopP "//x" ~?= Right (Divide, "x"),
        "eq" ~: parse bopP "==" ~?= Right (Eq, ""),
        "eq remainder" ~: parse bopP "==x" ~?= Right (Eq, "x"),
        "gt" ~: parse bopP ">" ~?= Right (Gt, ""),
        "ge before gt" ~: parse bopP ">=" ~?= Right (Ge, ""),
        "lt" ~: parse bopP "<" ~?= Right (Lt, ""),
        "le before lt" ~: parse bopP "<=" ~?= Right (Le, ""),
        "concat" ~: parse bopP ".." ~?= Right (Concat, ""),
        "concat remainder" ~: parse bopP "..x" ~?= Right (Concat, "x")
      ]

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
statementP = ifP `orP` fmap Return expP
  where
    ifP :: LuParser Statement
    ifP = do
      string "if "
      cond <- parens expP
      string " then "
      st1 <- parens statementP
      string " else "
      st2 <- parens statementP
      return $ If cond st1 st2

parseLuFile :: String -> IO (Either LuParseError Statement)
parseLuFile = parseAllFromFile statementP

testStatementP :: Test
testStatementP =
  "statementP"
    ~: TestList
      [ "return int" ~: parse statementP "42" ~?= Right (Return (Val (IntVal 42)), ""),
        "return expr" ~: parse statementP "(3) + (4)" ~?= Right (Return (Op2 (Val (IntVal 3)) Plus (Val (IntVal 4))), ""),
        "if simple" ~: parse statementP "if (True) then (1) else (2)" ~?= Right (If (Val (BoolVal True)) (Return (Val (IntVal 1))) (Return (Val (IntVal 2))), ""),
        "if nested" ~: parse statementP "if ((1) < (2)) then (True) else (False)" ~?= Right (If (Op2 (Val (IntVal 1)) Lt (Val (IntVal 2))) (Return (Val (BoolVal True))) (Return (Val (BoolVal False))), ""),
        "roundtrip return" ~: let s = Return (Op1 Neg (Val (IntVal 5))) in parse statementP (show s) ~?= Right (s, ""),
        "roundtrip if" ~: let s = If (Val (BoolVal True)) (Return (Val (IntVal 0))) (Return (Val (IntVal 1))) in parse statementP (show s) ~?= Right (s, "")
      ]

testParseLuFile :: IO Test
testParseLuFile = do
  simple <- parseLuFile "testFiles/statement.txt"
  arith <- parseLuFile "testFiles/complex_arith.txt"
  negs <- parseLuFile "testFiles/negations.txt"
  boolComp <- parseLuFile "testFiles/bool_comparison.txt"
  strConc <- parseLuFile "testFiles/string_concat.txt"
  nilVal <- parseLuFile "testFiles/nil_val.txt"
  lenCheck <- parseLuFile "testFiles/len_check.txt"
  notCheck <- parseLuFile "testFiles/not_check.txt"
  failure <- parseLuFile "testFiles/nonexistent.txt"
  return $
    TestList
      [ "simple int" ~: simple ~?= Right (Return (Val (IntVal 42))),
        "complex arith" ~: arith ~?= Right (Return (Op2 (Op2 (Op2 (Val (IntVal 1)) Plus (Val (IntVal 2))) Times (Op2 (Val (IntVal 3)) Minus (Val (IntVal 4)))) Divide (Op2 (Op2 (Val (IntVal 5)) Plus (Val (IntVal 6))) Times (Op2 (Val (IntVal 7)) Minus (Val (IntVal 8)))))),
        "negations" ~: negs ~?= Right (Return (Op2 (Op2 (Op2 (Op1 Neg (Val (IntVal 1))) Plus (Op1 Neg (Val (IntVal 2)))) Times (Op2 (Op1 Neg (Val (IntVal 3))) Plus (Op1 Neg (Val (IntVal 4))))) Plus (Op2 (Op2 (Op1 Neg (Val (IntVal 5))) Times (Val (IntVal 6))) Minus (Op2 (Val (IntVal 7)) Times (Op1 Neg (Val (IntVal 8))))))),
        "bool comparison" ~: boolComp ~?= Right (If (Op2 (Val (IntVal 1)) Lt (Val (IntVal 3))) (Return (Val (BoolVal True))) (Return (Val (BoolVal False)))),
        "string concat" ~: strConc ~?= Right (Return (Op2 (Op2 (Val (StringVal "hello")) Concat (Val (StringVal " world"))) Concat (Val (StringVal "!")))),
        "nil val" ~: nilVal ~?= Right (If (Op2 (Val (IntVal 5)) Ge (Val (IntVal 3))) (Return (Val NilVal)) (Return (Val (BoolVal False)))),
        "len check" ~: lenCheck ~?= Right (If (Op2 (Op1 Len (Val (StringVal "hello"))) Gt (Val (IntVal 3))) (Return (Val (BoolVal True))) (Return (Val NilVal))),
        "not as or" ~: notCheck ~?= Right (If (Op1 Not (Val (BoolVal True))) (Return (Val (BoolVal True))) (Return (Op1 Not (Val (BoolVal False))))),
        "failure" ~: failure ~?= Left "testFiles/nonexistent.txt: openFile: does not exist (No such file or directory)"
      ]

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = 2.5

question :: String
question = "are parsers in other languages also this hard to implement? ;-;"

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
  fileTests <- testParseLuFile
  _ <-
    runTestTT $
      TestList
        [ check,
          testKeywordP,
          testParens,
          testListP,
          testValueP,
          testExpP,
          testUopP,
          testBopP,
          testStatementP,
          fileTests
        ]
  return ()

--------------------------

{-

COPY CODE FROM PREVIOUS WEEKS HERE

Definitions needed for intP have been stubbed.

-}

instance Functor LuParser where
  fmap :: (a -> b) -> LuParser a -> LuParser b
  fmap f p = LuParser $ \s -> fmap (\(out, rem) -> (f out, rem)) (parse p s)

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

filterParse :: (Show a) => (a -> Bool) -> LuParser a -> LuParser a
filterParse func p = LuParser $ \s -> do
  (out, rem) <- parse p s
  if func out
    then return (out, rem)
    else Left ("Predicate failed on: " ++ show out)

charWhere :: (Char -> Bool) -> LuParser Char
charWhere func = LuParser $ \s -> case s of
  [] -> Left "No characters left."
  (x : xs) -> if func x then Right (x, xs) else Left ("Predicate failed on: " ++ show x)

char :: Char -> LuParser Char
char c = charWhere (== c)

string :: String -> LuParser String
string = mapM char

digit :: LuParser Char
digit = charWhere isDigit

orP :: LuParser a -> LuParser a -> LuParser a
orP parser1 parser2 = LuParser $ \s -> case parse parser1 s of
  Left err_str -> parse parser2 s
  Right output -> Right output

eof :: LuParser ()
eof = LuParser $ \s -> case s of
  [] -> Right ((), s)
  x : xs -> Left "Not yet end of file. "

betweenP :: LuParser a -> LuParser b -> LuParser c -> LuParser b
betweenP p1 p2 p3 = do
  _ <- p1
  o2 <- p2
  _ <- p3
  return o2

parseAll :: LuParser a -> LuParser a
parseAll p = betweenP (pure ()) p eof

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
  deriving (Eq, Ord)

instance Show Value where
  show NilVal = "nil"
  show (IntVal x) = show x
  show (BoolVal x) = show x
  show (StringVal x) = show x

data Uop
  = Neg
  | Not
  | Len
  deriving (Eq)

instance Show Uop where
  show Neg = "-"
  show Not = "not"
  show Len = "#"

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

data Expression
  = Val Value
  | Op1 Uop Expression
  | Op2 Expression Bop Expression
  deriving (Eq)

instance Show Expression where
  show (Val value) = show value
  show (Op1 uop exp) = show uop ++ "(" ++ show exp ++ ")"
  show (Op2 exp1 bop exp2) = "(" ++ show exp1 ++ ") " ++ show bop ++ " (" ++ show exp2 ++ ")"

data Statement
  = If Expression Statement Statement
  | Return Expression
  deriving (Eq)

instance Show Statement where
  show (If exp st1 st2) = "if (" ++ show exp ++ ") then (" ++ show st1 ++ ") else (" ++ show st2 ++ ")"
  show (Return st) = show st