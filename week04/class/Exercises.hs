module Exercises where

import Data.Char (isSpace, isUpper)

-- safeHead example

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

-- lengths example

lengths :: [[a]] -> [Int]
lengths = map length

-- singletons example

singletons :: [a] -> [[a]]
singletons = map (: [])

-- squares example

squares :: [Int] -> [Int]
squares = map (^ 2)

-- foo example

foo :: (a -> Bool) -> [a] -> [a]
foo fun [] = []
foo fun (x : xs)
  | fun x = x : foo fun xs
  | otherwise = foo fun xs

-- removeSpaces example

removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

-- reverse example

reverse :: [a] -> [a]
reverse = foldr (\x y -> y ++ [x]) []

-- Exercise

-- Store a person's name and age.
data Person = Person String Int
  deriving (Show)

-- Fill in these two functions:
-- One to get a person's name, and one to get their age.

name :: Person -> String
name (Person x y) = x

age :: Person -> Int
age (Person x y) = y

-- Return the names of all people in the input list under age 18.
-- Use map and filter.
youngNames :: [Person] -> [String]
youngNames = map name . filter (\x -> age x < 18)

-- youngNames peopleInput should return ["Bob", "Jill"].

peopleInput :: [Person]
peopleInput = [Person "Bob" 12, Person "Jack" 23, Person "Jill" 18, Person "Alice" 70]

-- Exercise 2:

-- Reimplement map and filter using foldr.

map' :: (a -> b) -> [a] -> [b]
map' fun = foldr (\x y -> fun x : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' fun = foldr (\x y -> (if fun x then x : y else y)) []