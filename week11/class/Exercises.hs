{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Exercises where

import Control.Monad.State.Lazy
import Test.HUnit (Assertion, Test, assertEqual, (~:))

{-
Exercise:
Write first xs, then ys to the terminal.
-}
putTwo :: String -> String -> IO ()
putTwo xs ys = undefined

{-
Exercise:
Ask the user for two numbers and print their sum.
Make sure to check that it works using the terminal.
-}

addTwo :: IO ()
addTwo = undefined

{-
Exercise:
Given an integer n, ask the user to repeatedly
guess a number until they get n. Tell them if their
guess is higher or lower than expected. If they get
the right answer, tell them so and end the game.
Note: max and min values for Int are given by
maxBound and minBound.
-}

guessGame :: Int -> IO ()
guessGame = undefined

{-
Exercise:

Take a look
[here](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#g:29).
Observe that `FilePath` is just a _type synonym_ for `String`. They're entirely
interchangeable, but we use a new name for documentation purposes and for
maintainability, in case we choose to change the implementation later.

Try using `readFile` and `writeFile` a bit. Figure out if the following
statements are true or false, and fill in the corresponding variable
in `Exercises.hs`.

1. `readFile` will throw an exception if given the name of a file that does
   not exist.

2. `writeFile` will throw an exception if given the name of a file that does
   not exist.

3. `writeFile` will overwrite the contents of an existing file.-}

q1 :: Bool
q1 = undefined

q2 :: Bool
q2 = undefined

q3 :: Bool
q3 = undefined

{-
Exercise

Practice using `readFile` and `writeFile` by implementing the short functions
below. Do **not** use any recursive functions in your solutions to this
exercise.

(a)

`lengthFile` should return the number of characters (including whitespace)
in the contents of the input file.

(b)

`concatFiles` takes as parameters a list of input files and an output file. It
should concatenate the contents of the input files and write those contents
into the output file.

You should use mapM.
-}

lengthFile :: FilePath -> IO Int
lengthFile = undefined

testLengthFile :: Test
testLengthFile =
  "lengthFile"
    ~: [ testLength "two/input1.txt" 20,
         testLength "two/input2.txt" 0,
         testLength "two/input3.txt" 11
       ]

concatFiles :: [FilePath] -> FilePath -> IO ()
concatFiles = undefined

testConcatFiles :: Test
testConcatFiles =
  "concatFiles"
    ~: [ testConcat
           ["two/input1.txt", "two/input2.txt", "two/input3.txt"]
           "two/result.txt"
           "two/expected.txt"
       ]

parseChar :: State String (Maybe Char)
parseChar = undefined

{-
Exercise

In this exercise, you will reimplement some `IO` functions for printing.
Note that `putChar :: Char -> IO ()` takes a character and prints it.

1. (Re)implement `putStr` in terms of `putChar`.
   Do it twice: first with a recursive function (`putStr'`), then with `mapM_`
   (`putStr''`).

2. (Re)implement `putStrLn` in terms of `putStr'`.

3. (Re)implement `print` in terms of `putStrLn'`.

If you are not sure how some of these functions work, refer to the documentation
[here](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#g:27)
or try them out in GHCi.

To test your reimplementations, make sure you get the same output on these
commands in GHCi:

```
ghci> putStr' "hello, world!"
hello, world!ghci> putStrLn' "hello again"
hello again
ghci> print' 3
3
```
-}

putStr' :: String -> IO ()
putStr' = error "unimplemented"

putStr'' :: String -> IO ()
putStr'' = error "unimplemented"

putStrLn' :: String -> IO ()
putStrLn' = error "unimplemented"

print' :: (Show a) => a -> IO ()
print' = error "unimplemented"

-- Utils for testing

testLength :: FilePath -> Int -> Assertion
testLength input n = do
  n' <- lengthFile input
  assertEqual "" n n'

testConcat :: [FilePath] -> FilePath -> FilePath -> Assertion
testConcat inputs output expected = do
  oute <- readFile expected
  concatFiles inputs output
  outr <- readFile output
  assertEqual "" oute outr
