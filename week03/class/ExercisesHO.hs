module Exercises where

import Data.List (sort)
import Prelude hiding (and, sum)

-- Eta reduce this function as much as possible

absAll :: [Int] -> [Int]
absAll xs = map (\x -> abs x) xs

-- Rewrite to use function composition and eta reduce

desort :: [Int] -> [Int]
desort xs = reverse (sort xs)

oddOnly :: [Int] -> [Int]
oddOnly xs = filter (\x -> not (even x)) xs

evenOdds :: [Int] -> Bool
evenOdds xs = even (length (oddOnly xs))

and :: Bool -> Bool -> Bool
and x y = x && y

-- Rewrite to use operator sections and eta reduce

add3All :: [Int] -> [Int]
add3All xs = map (\x -> 3 + x) xs

greaterThan100 :: [Int] -> [Int]
greaterThan100 xs = filter (\x -> x > 100) xs

--------------

-- Rewrite

twentyYOPeopleNames :: [Person] -> [String]
twentyYOPeopleNames xs = map name (filter (\x -> age x == 20) xs)

data Person = Person String Int
  deriving (Show)

name :: Person -> String
name (Person s _) = s

age :: Person -> Int
age (Person _ n) = n
