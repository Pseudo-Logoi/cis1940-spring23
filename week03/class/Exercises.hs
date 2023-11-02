module Exercises where

-- Exercise 1:

-- Store a person's name and age.
data Person = Person String Int
  deriving (Show)

{- Get the names of the people in a list
   who are at most 18 years old.
-}
youngNames :: [Person] -> [String]
-- youngNames people =
--   map (\(Person name _) -> name) (filter (\(Person _ age) -> age <= 18) people)
youngNames people = map getName (filter isYoung people)
  where
    getName :: Person -> String
    getName (Person name _) = name

    isYoung :: Person -> Bool
    isYoung (Person _ age) = age <= 18

{- As usual, in a terminal in this folder, run
   $ stack ghci Exercises.hs

   Then, at the prompt, you can evaluate your code!
   For example,
   > youngNames peopleInput

   should return ["Bob", "Jill"]
-}

peopleInput :: [Person]
peopleInput = [Person "Bob" 12, Person "Jack" 23, Person "Jill" 18, Person "Alice" 70]

-- Exercise 2:

-- Reimplement map and filter using foldr.

map' :: (a -> b) -> [a] -> [b]
map' f as = foldr (\a bs -> f a : bs) [] as

filter' :: (a -> Bool) -> [a] -> [a]
filter' f as = foldr (\a bs -> if f a then a : bs else bs) [] as