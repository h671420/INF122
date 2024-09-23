module Week37 where

import Prelude hiding (and, concat, replicate, (!!), elem)
import Data.Char

euclid :: Int -> Int -> Int
euclid a b  | a==b = a
            | otherwise = euclid (min a b) (max a b - min a b)

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x&&and xs


concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss


replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate nums ele = ele:replicate (nums-1) ele


(!!) :: [a] -> Int -> a
li !! ind
    | ind == 0 = head li
    | otherwise = tail li !! (ind -1)


elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem el (x:xs)
    | el == x = True
    | otherwise = elem el xs


merge :: (Ord a) => [a] -> [a] -> [a]
merge a [] = a
merge [] a = a
merge (a:as) (b:bs) | a<b = a: merge as (b:bs)
                    |otherwise = b: merge (a:as) bs

half :: [a] -> ([a],[a])
half li = splitAt (div (length li) 2) li

msort :: (Ord a) => [a] -> [a]
msort li    | length li <2 = li
            | otherwise = merge (msort (fst (half li))) (msort (snd (half li)))



tokenise :: String -> [String]
tokenise [] = []
tokenise str
    |head str == '+' = [head str]:tokenise (tail str)
    |head str == '-' = [head str]:tokenise (tail str)
    |head str == '*' = [head str]:tokenise (tail str)
    |head str == '/' = [head str]:tokenise (tail str)
    |head str == '(' = [head str]:tokenise (tail str)
    |head str == ')' = [head str]:tokenise (tail str)
    |isDigit (head str) = takeWhile isDigit str:tokenise (dropWhile isDigit str)
    |isLetter (head str) = takeWhile isLetter str:tokenise (dropWhile isLetter str)
    |otherwise = tokenise (tail str)
