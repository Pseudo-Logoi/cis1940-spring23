module Exercises where

import Test.HUnit (Test (TestList), runTestTT, (~?=))

{-
Exercise 0: Uncomment the code below by removing the two hyphens.
It should give you a type error, e.g. a red underline under + with an error message
that appears when you hover. Comment out the code again when you've confirmed.
通过删除两个连字符来取消对下面代码的注释。
它应该会给你一个类型错误，例如在+下有一个红色下划线，当你悬停时会显示一条错误消息。确认后再次注释代码。
-}
-- foo = 1 + "hello"

{-
Exercise 1: Remove the comments that say [ORMOLU_DISABLE] and [ORMOLU_ENABLE].
When you save the file, the extra spaces before the 3 should disappear.
删除上面写着[ORMOLO_DISABLE]和[ORMOLU_ENABLE]的注释。保存文件时，3之前的多余空格应该会消失。
-}
format :: Int
format = 3

{-
Exercise 2: The following code should have an underline and an hlint suggestion
should appear when you hover. Click [Quick Fix], then [Apply hint "Redundant bracket"].
以下代码应该有下划线，悬停时应该显示一个hlint建议。单击[Quick Fix]，然后单击[Apply hint "Redundant bracket" 冗余括号]。
-}
linter :: Int
linter = 3 + 4

{-
Exercise 3: Replace undefined with 1940.
-}
n :: Int
n = 1940

ntest :: Test
ntest = n ~?= 1940

main :: IO ()
main = do
  print "You have installed Haskell!"
  _ <- runTestTT $ TestList [ntest]
  return ()