{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
module Exercises where

import Control.Monad.Identity (Identity)

{-
There are standard library versions of these in Control.Monad.State and
Control.Monad.Trans, but we will use our own definitions for now.

Note: EitherT is also called ExceptT in the standard library. These are
equivalent, just defined in different modules.
-}
newtype State s a = MkState {runState :: s -> (a, s)}

newtype StateT s m a = MkStateT {runStateT :: s -> m (a, s)}

newtype EitherT e m a = MkEitherT {runEitherT :: m (Either e a)}

type ParserError = String

-- type Parser a =

-- type Parser' a =

-- char :: Char -> Parser Char
-- char c = MkStateT $ \s -> case s of
--   (c' : cs) ->
--     if c' == c
--       then return (c, cs)
--       else Left $ "Cannot parse " ++ [c'] ++ " as " ++ [c] ++ "."
--   [] -> Left $ "Cannot parse empty string as " ++ [c] ++ "."

-- type State' s a =

{-
Bonus exercises (each has a standard library definition you can check against):

1. Define MaybeT.
2. Define a typeclass MonadError capturing the idea of what it means for a
    monad (Either a, EitherT e m, etc.) to model error behavior.
3. Define a typeclass MonadState capturing the idea of what it means for a
    monad (State s, StateT s m, etc.) to model error behavior.
4. Write instance declarations for the typeclasses in 2 & 3.
-}