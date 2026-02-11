module Exercises where

import Data.List (sort)
import Prelude hiding (and, sum)

-- Eta reduce this function as much as possible

absAll :: [Int] -> [Int]
absAll = map abs

-- Rewrite to use function composition and eta reduce

desort :: [Int] -> [Int]
desort = reverse . sort

oddOnly :: [Int] -> [Int]
oddOnly = filter (not . even)

evenOdds :: [Int] -> Bool
evenOdds = even . length . oddOnly

and :: Bool -> Bool -> Bool
and = (&&)

-- Rewrite to use operator sections and eta reduce

add3All :: [Int] -> [Int]
add3All = map (3 +)

greaterThan100 :: [Int] -> [Int]
greaterThan100 = filter (> 100)

--------------

-- Rewrite

twentyYOPeopleNames :: [Person] -> [String]
twentyYOPeopleNames = map name . filter ((== 20) . age)

data Person = Person String Int
  deriving (Show)

name :: Person -> String
name (Person s _) = s

age :: Person -> Int
age (Person _ n) = n
