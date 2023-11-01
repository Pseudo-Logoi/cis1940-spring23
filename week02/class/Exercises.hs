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

{- Check whether a number is in the tree. For example,
   [find 1 tree] and [find 2 tree] should be [True], while
   [find 3 tree] should be [False]. -}

{- Add three to every node value in the tree. For example,
   [add3 tree] should be [Node Leaf 4 (Node Leaf 5 Leaf)]. -}