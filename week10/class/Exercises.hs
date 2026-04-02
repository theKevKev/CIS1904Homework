module Exercises where

import Data.List (elemIndex, lookup)
import Prelude hiding (mapM)

foo :: (Applicative f) => (a -> f b) -> f a -> f b
foo = undefined

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead _ = Nothing

safeHeadMaybe :: Maybe [a] -> Maybe a
safeHeadMaybe = undefined

bar :: (Applicative f) => f (c -> d) -> f c -> f d
bar g x = undefined

addFirsts :: [Int] -> [Int] -> Maybe Int
addFirsts xs ys = case safeHead xs of
  Nothing -> Nothing
  Just x -> case safeHead ys of
    Nothing -> Nothing
    Just y -> Just (x + y)

addFirstsEven :: [Int] -> [Int] -> Maybe Int
addFirstsEven xs ys = undefined

{-
For each of fish and join, first implement it without do notation, then with.
-}

fish1 :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
fish1 = error "unimplemented"

fish2 :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
fish2 = error "unimplemented"

join1 :: (Monad m) => m (m a) -> m a
join1 = error "unimplemented"

join2 :: (Monad m) => m (m a) -> m a
join2 = error "unimplemented"

{-
Re-implement the `Monad` library function `when`, which supports
conditional execution of a monadic computation — when the boolean flag is
`True`, then we return that computation; otherwise, we return the unit value
(i.e., a computation that does nothing).
-}

when :: (Monad m) => Bool -> m () -> m ()
when = error "unimplemented"

{-
Exercise:
Get the third element of a list. Use safeHead and safeTail.
e.g. [1, 2, 3, 4] -> Just 3 and [1, 2] -> Nothing.
-}

safeThird :: [a] -> Maybe a
safeThird = error "unimplemented"

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

{-
Exercise:
Compute the factors of a number. Try it with each of: regular monad syntax
(i.e., return and >>=), do notation, and list comprehension syntax.
e.g. 15 -> [1, 3, 5, 15]
-}

factors1 :: Int -> [Int]
factors1 = error "unimplemented"

factors2 :: Int -> [Int]
factors2 = error "unimplemented"

factors3 :: Int -> [Int]
factors3 = error "unimplemented"

-------

{-
Exercise:
Implement mapM using sequence (and another function).
-}

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM = error "unimplemented"