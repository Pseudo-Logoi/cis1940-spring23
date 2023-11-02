module Exercises where

import Data.List
import Test.HUnit
import Prelude hiding (any, or)

{- Read [instructions.md] first. -}

-- Exercise 1:

or' :: [Bool] -> Bool
or' bools = foldr (||) False bools

exercise1a :: Test
exercise1a =
  "or"
    ~: [ or' [True] ~?= True,
         or' [False] ~?= False,
         or' [True, False, False, True] ~?= True,
         or' [False, False, False] ~?= False
       ]

any' :: (a -> Bool) -> [a] -> Bool
any' f as = or' (map f as)

exercise1b :: Test
exercise1b =
  "any1"
    ~: [ any' id [] ~?= False,
         any' even [1] ~?= False,
         any' even [1, 2, 3] ~?= True,
         any' not [False, False] ~?= True
       ]

any'' :: (a -> Bool) -> [a] -> Bool
any'' f as = foldr (\a b -> f a || b) False as

exercise1c :: Test
exercise1c =
  "any2"
    ~: [ any'' id [] ~?= False,
         any'' even [1] ~?= False,
         any'' even [1, 2, 3] ~?= True,
         any'' not [False, False] ~?= True
       ]

-- Exercise 2:

silly :: String -> [String]
silly = error "unimplemented"

exercise2 :: Test
exercise2 =
  "silly"
    ~: [ silly "" ~?= [],
         silly "a aa aaa aaaa" ~?= [],
         silly "b" ~?= ["b"],
         silly "I have loads of fun words"
           ~?= ["I", "o,f", "f,u,n", "w,o,r,d,s"],
         silly " it's   okay  if  there are  symbols! and spaces  "
           ~?= ["i,t,',s", "i,f", "t,h,e,r,e", "s,y,m,b,o,l,s,!"]
       ]

---- end of exercises ----

{- Write down the approximate number of hours
it took you to complete this homework. If you have any
comments, feel free to also write them here. -}

time :: Double
time = error "unimplemented"

checkTime :: Test
checkTime = TestCase (assertBool "fill in any time" (time >= 0))

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1a,
          exercise1b,
          exercise2,
          checkTime
        ]
  return ()