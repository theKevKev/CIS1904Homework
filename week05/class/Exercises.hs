{-# LANGUAGE BangPatterns #-}

module Exercises where

data Stream a = Cons a (Stream a)

streamTake :: Int -> Stream a -> [a]
streamTake i (Cons x xs)
  | i <= 0 = []
  | otherwise = x : streamTake (i - 1) xs

{- Make a Stream from a list. If the list is finite, all remaining elements
    have the default value.-}

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamFromList :: a -> [a] -> Stream a
streamFromList x = foldr Cons (streamRepeat x)