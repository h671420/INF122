--OK
module MyFactorial where

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct  (x:xs) = x * myProduct xs

{-
product [2,3,4]
2 * product [3,4]
2 * 3 * product [4]
2 * 3 * 4 * product []
2 * 3 * 4 * 1
6 * 4 * 1 = 24
-}