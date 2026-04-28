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

instance Arbitrary Value where
  arbitrary :: Gen Value
  arbitrary = oneof [genNilVal, genIntVal, genBoolVal, genStringVal]
    where
      genNilVal = return NilVal

      genIntVal = fmap IntVal arbitrary

      genBoolVal = fmap BoolVal arbitrary

      genStringVal = fmap StringVal $ listOf (suchThat arbitrary isAllowedChar)

  shrink :: Value -> [Value]
  shrink NilVal = []
  shrink (IntVal x) = map IntVal (shrink x)
  shrink (BoolVal x) = map BoolVal (shrink x)
  shrink (StringVal x) = map StringVal (filter (all isAllowedChar) (shrink x))

isAllowedChar :: Char -> Bool
isAllowedChar c = isDigit c || isAsciiLower c || isAsciiUpper c

{-
Exercise 2

Write Arbitrary instances for Uop and Bop.
(Consider whether it makes sense to implement shrink here.)
-}

instance Arbitrary Uop where
  arbitrary :: Gen Uop
  arbitrary = elements [Neg, Not, Len]

instance Arbitrary Bop where
  arbitrary :: Gen Bop
  arbitrary = elements [Plus, Minus, Times, Divide, Eq, Gt, Ge, Lt, Le, Concat]

{-
Exercise 3
Write Arbitrary instances for Expression and Statement.
Your definition of arbitrary for each should use sized.

Be sure to implement both arbitrary and shrink.
-}

instance Arbitrary Expression where
  arbitrary :: Gen Expression
  arbitrary = sized genExpression
    where
      genExpression n | n <= 0 = fmap Val arbitrary
      genExpression n = frequency [(3, genValue), (2, genUop), (1, genBop)]
        where
          genValue = fmap Val arbitrary
          genUop = fmap Op1 arbitrary <*> genExpression (n - 1)
          genBop = fmap Op2 (genExpression (n - 1)) <*> arbitrary <*> genExpression (n - 1)

  shrink :: Expression -> [Expression]
  shrink (Val value) = map Val (shrink value)
  shrink (Op1 uop exp) = exp : map (Op1 uop) (shrink exp)
  shrink (Op2 exp1 bop exp2) =
    exp1 : exp2 : do
      (exp1s, exp2s) <- shrink (exp1, exp2)
      return $ Op2 exp1s bop exp2s

instance Arbitrary Statement where
  arbitrary :: Gen Statement
  arbitrary = sized genStatement
    where
      genStatement n | n <= 0 = fmap Return arbitrary
      genStatement n = oneof [genIf, genReturn]
        where
          genIf = fmap If arbitrary <*> genStatement (n - 1) <*> genStatement (n - 1)
          genReturn = fmap Return arbitrary

  shrink :: Statement -> [Statement]
  shrink (Return exp) = fmap Return (shrink exp)
  shrink (If exp stt1 stt2) =
    stt1 : stt2 : do
      (exps, stt1s, stt2s) <- shrink (exp, stt1, stt2)
      return $ If exps stt1s stt2s

{-
Exercise 4

Write roundtrip properties to test that your parsers for values, expressions,
and statements are left inverses of your implementation of show for each.
-}

prop_roundtrip_value :: Value -> Bool
prop_roundtrip_value v = parse (parseAll valueP) (show v) == Right (v, "")

prop_roundtrip_expression :: Expression -> Bool
prop_roundtrip_expression e = parse (parseAll expP) (show e) == Right (e, "")

prop_roundtrip_statement :: Statement -> Bool
prop_roundtrip_statement s = parse (parseAll statementP) (show s) == Right (s, "")

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = 2

question :: String
question = "So if property based testing started in Haskell, what are the challenges in setting up property-based tests in other languages? Is it easier? Harder? Better? Less effective?"

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

listP :: LuParser a -> LuParser [a]
listP p = orP (neListP p) (pure [])
  where
    -- parse a nonempty list of elements matched by p
    neListP :: LuParser a -> LuParser [a]
    neListP p' = do
      out <- p'
      rem <- listP p'
      return (out : rem)

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

keywordP :: String -> a -> LuParser a
keywordP keyword value = fmap (const value) (string keyword)

parens :: LuParser a -> LuParser a
parens p = betweenP (char '(') p (char ')')

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