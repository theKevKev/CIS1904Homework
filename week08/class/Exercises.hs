{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Data.List
import GHC.Natural (Natural)
import Test.HUnit
import Test.QuickCheck (Arbitrary (arbitrary), Property, (===))

{- We will work with polynomials in this exercise.

In our representation, instead of explicitly specifying the degrees of terms,
we represent a polynomial as a list of coefficients, each of which has degree
equal to its index in the list.

```Haskell
newtype Poly = P [Int]
```

In this representation, the polynomial `3 + 5x + x^2` would be written as
`P [3, 5, 1]`.

\**Note**: You may use functions from `Data.List` in this exercise.
-}

newtype Poly = P [Int]

-- deriving (Eq) -- Erase this.

{-
Erase the Eq derivation above and write a new Eq instance for Poly here.
-}

instance Eq Poly where
  (==) :: Poly -> Poly -> Bool
  P xs == P ys = dropWhileEnd (== 0) xs == dropWhileEnd (== 0) ys

{-
What is a number? In Haskell, a number is any type that is an instance of the
`Num` typeclass. Polynomials can be added, multiplied, and so on, just like any
other number. We will write an instance of `Num` for `Poly` over the course of
the remaining exercises.

First, implement `negate`, which should negate all of the coefficients of the
polynomial. For example, `P [1, 2, 3]` becomes `P [-1, -2, -3]`.

Remember that you can use `negate` on any type that is an instance of `Num`
type class, including for the `Int` coefficients in the list.

-}

instance Num Poly where
  (+) :: Poly -> Poly -> Poly
  (+) = plus -- Implemented later

  (*) :: Poly -> Poly -> Poly
  (*) = times -- Implemented later

  negate :: Poly -> Poly
  negate (P xs) = P (map negate xs)

  fromInteger :: Integer -> Poly
  fromInteger = fromIntegerPoly -- Implemented later

  -- These have multiple potential definitions; we have
  -- chosen one and implemented it for you. (Num is an old
  -- typeclass; not all of these functions make sense for types
  -- for which we might reasonably want, say, +, -, and *.)
  abs :: Poly -> Poly
  abs = id

  signum :: Poly -> Poly
  signum x = if x == P [] then P [] else P [1]

testNegate :: Test
testNegate =
  "negate"
    ~: [ negate (P [1, 2, 3]) ~?= P [-1, -2, -3],
         negate (P [-1, 0]) ~?= P [1, 0],
         negate (P []) ~?= P []
       ]

{-Next, we define polynomial addition. We need to add the coefficients pointwise
for each term in the two polynomials.

For example, `(5 + x) + (1 + x + 3x^2) = 6 + 2x + 3x^2`. In terms of our
representation, this means that `P [5, 1] + P [1, 1, 3] = P [6, 2, 3]`.
-}

plus :: Poly -> Poly -> Poly
plus (P []) q = q
plus p (P []) = p
plus (P (x : xs)) (P (y : ys)) = let P rest = plus (P xs) (P ys) in P ((x + y) : rest)

testPlus :: Test
testPlus =
  "plus"
    ~: [ P [0, 1, 2] + P [1, 0, 2] ~?= P [1, 1, 4],
         P [5, 1] + P [1, 1, 3] ~?= P [6, 2, 3],
         P [1, 1, 3] + P [5, 1] ~?= P [6, 2, 3],
         P [] + P [] ~?= P []
       ]

{-Next, we define polynomial multiplication. To multiply two polynomials, each
term in the first polynomial must be multiplied by each term in the second
polynomial. The easiest way to achieve this is to build up a `[Poly]` where
each element is the polynomial resulting from multiplying a single coefficient
in the first polynomial by each coefficient in the second polynomial.

Since the terms do not explicitly state their exponents, you will have to shift
the output before multiplying it by each consecutive coefficient. For example
`P [1, 1, 1] * P [2, 2]` will yield the list
`[P [2, 2], P [0, 2, 2], P [0, 0, 2, 2]]`. You can then simply `sum` this list,
since we've already defined `(+)`.
-}

times :: Poly -> Poly -> Poly
times (P xs) (P ys) = sum $ zipWith shiftMult xs [0 ..]
  where
    shiftMult c i = P (replicate i 0 ++ map (* c) ys)

testTimes :: Test
testTimes =
  "times"
    ~: [ P [1, 1, 1] * P [2, 2] ~?= P [2, 4, 4, 2],
         P [2, 2] * P [1, 1, 1] ~?= P [2, 4, 4, 2],
         P [1, 2, 3] * P [4, 5, 6] ~?= P [4, 13, 28, 27, 18],
         P [] * P [1, 2, 3] ~?= P []
       ]

{- Implement `fromInteger`, which converts an integer into a degree zero
polynomial. For example, `3` becomes `P [3]`. -}

fromIntegerPoly :: Integer -> Poly
fromIntegerPoly n = P [fromInteger n]

testFromInteger :: Test
testFromInteger =
  "fromInteger"
    ~: [ 3 ~?= P [3],
         0 ~?= P [0]
       ]

{-
Now that we've finished implementing the `Num` instance, we can write and
manipulate polynomials very similarly to how we would in mathematical notation.
See `final` for an example. Note that though we did not implement `^` and `-`
explicitly, these are automatically defined for us in terms of the functions we
did define.
-}

x :: Poly
x = P [0, 1]

final :: Test
final = (2 * x - 1) * (x + 2) == (2 * x ^ 2 + 3 * x - 2) ~?= True

{-
Finally, we will write an instance of `Show` for `Poly`. For example,
`P [1, 2, 3]` should be displayed as `1 + 2x + 3x^2`.

It should satisfy these constraints:

-   Terms are displayed as `cx^d` where `c` is the coefficient and `d` is the
    degree.
    + If `d` is 0, then only the coefficient is displayed.
    + If `d` is 1, then the format is simply `cx`.
    + If `c` is 0, omit the term (unless there are no nonzero terms; see the
        third bullet point).

-   Terms are separated by the `+` sign with a single space on each side of it.

-   As a special case, for a polynomial with no nonzero coefficients, display
    `0` rather than the empty string.
-}

instance Show Poly where
  show :: Poly -> String
  show (P xs) =
    let nonZero = filter ((/= 0) . fst) (zip xs [0 ..])
        showTerm (c, 0) = show c
        showTerm (1, 1) = "x"
        showTerm (c, 1) = show c ++ "x"
        showTerm (1, d) = "x^" ++ show d
        showTerm (c, d) = show c ++ "x^" ++ show d
        joinRest (c, d)
          | c < 0 = " - " ++ showTerm (abs c, d)
          | otherwise = " + " ++ showTerm (c, d)
     in case nonZero of
          [] -> "0"
          (t : ts) -> showTerm t ++ concatMap joinRest ts

testShow :: Test
testShow =
  "show"
    ~: [ show (P [1, 2, 3]) ~?= "1 + 2x + 3x^2",
         show (P [-1, 0]) ~?= "-1",
         show (P []) ~?= "0",
         show (P [0]) ~?= "0",
         show (P [5, 0, 2]) ~?= "5 + 2x^2",
         show (P [0, 4]) ~?= "4x",
         show (P [2, 1]) ~?= "2 + x",
         show (P [2, -1]) ~?= "2 - x"
       ]

------------------------------------------

instance Arbitrary Poly where
  arbitrary = do
    P <$> arbitrary

prop_plusAssoc :: Poly -> Poly -> Poly -> Property
prop_plusAssoc x y z = (x + y) + z === x + (y + z)

prop_plusComm :: Poly -> Poly -> Property
prop_plusComm x y = x + y === y + x

prop_fromIntegerAddId :: Poly -> Property
prop_fromIntegerAddId x = x + fromInteger 0 === x

prop_negateAddInv :: Poly -> Property
prop_negateAddInv x = x + negate x === fromInteger 0

prop_timesAssoc :: Poly -> Poly -> Poly -> Property
prop_timesAssoc x y z = (x * y) * z === x * (y * z)

prop_fromIntegerMultId :: Poly -> Property
prop_fromIntegerMultId p = x * fromInteger 1 === x

prop_timesComm :: Poly -> Poly -> Property
prop_timesComm x y = x * y === y * x

prop_timesDist :: Poly -> Poly -> Poly -> Property
prop_timesDist x y z = x * (y + z) === x * y + x * z

prop_absSignum :: Poly -> Property
prop_absSignum x = abs x * signum x === x
