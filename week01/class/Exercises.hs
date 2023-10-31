module Exercises where

{- Part 1:
   Declarations -}

x :: Int
x = 3

-- Error: x = 4

{- Part 2:
   Basic Types -}

i :: Int
i = 3

j :: Int
j = 4

d :: Double
d = 3.14

f :: Float
f = 2.7

{- GHCi:
    > 3 + 4
    > 8.7 / 3.1
    > mod 19 3
    > 19 `mod` 3
    > i + j
-}

-- Error: bad = i + d

-- Error: bad = i / i

{- Part 3:
   Functions -}

{- Exercise:
   sumtorial n should evaluate to 0 + 1 + ... + n
   example: sumtorial 5 should be 15 -}
sumtorial :: Int -> Int
sumtorial n
  | n == 0 = 0
  | otherwise = n + sumtorial (n - 1)

{- Part 3:
   Lists -}

{- Exercise:
   double xs should double every element in xs
   example: double [1, 2, 3] should be [2, 4, 6] -}
double :: [Int] -> [Int]
double = map (2 *)
