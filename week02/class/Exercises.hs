module Exercises where

-- data types by example

data Weather
  = Sunny
  | Cloudy
  | Windy
  | Rainy
  | Snowy
  deriving (Show)

wea :: Weather
wea = Sunny

data Point = Point Int Int
  deriving (Show)

data WeatherRequest
  = ByLocation String
  | ByCoordinate Point
  deriving (Show)

weareq :: WeatherRequest
weareq = ByCoordinate (Point 2 3)

data WeatherResult
  = Valid Weather
  | Invalid
  deriving (Show)

-- recursive data types

data Tree
  = Leaf
  | Node Tree Int Tree
  deriving (Show)

tree :: Tree
tree = Node Leaf 1 (Node Leaf 2 Leaf)

{- Count the number of leaves in the tree. For example,
   [numLeaves tree] should be [3]. -}
numLeaves :: Tree -> Int
numLeaves Leaf = 1
numLeaves (Node l_child _ r_child) = numLeaves l_child + numLeaves r_child

{- Check whether a number is in the tree. For example,
   [find 1 tree] and [find 2 tree] should be [True], while
   [find 3 tree] should be [False]. -}
find :: Int -> Tree -> Bool
find val Leaf = False
find val (Node l_child root_val r_child) = val == root_val || find val l_child || find val r_child

{- Add three to every node value in the tree. For example,
   [add3 tree] should be [Node Leaf 4 (Node Leaf 5 Leaf)]. -}
add3 :: Tree -> Tree
add3 Leaf = Leaf
add3 (Node l_child root_val r_child) = Node (add3 l_child) (root_val + 3) (add3 r_child)

data Thing
  = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving (Show)

data Person = Person String Int Thing
  deriving (Show)

brent :: Person
brent = Person "Brent" 31 SealingWax

displayPerson :: Person -> String
displayPerson p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n