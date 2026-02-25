{-# LANGUAGE BangPatterns #-}

module Exercises where

data Stream a = Cons a (Stream a)

streamTake :: Int -> Stream a -> [a]
streamTake = error "unimplemented"

{- Make a Stream from a list. If the list is finite, all remaining elements
    have the default value.-}

streamRepeat :: a -> Stream a
streamRepeat = error "unimplemented"

streamFromList :: a -> [a] -> Stream a
streamFromList = error "unimplemented"
