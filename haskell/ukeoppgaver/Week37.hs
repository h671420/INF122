module Week37 where

import Prelude hiding (and, concat, replicate, (!!), elem)
import Data.Char

euclid :: Int -> Int -> Int
euclid a b  | a==b = a
            | otherwise = euclid ((max a b)- (min a b)) min a b

and :: [Bool] -> Bool
and = foldr (&&) True

concat :: [[a]] -> [a]
concat = undefined

replicate :: Int -> a -> [a]
replicate = undefined

(!!) :: [a] -> Int -> a
li !! ind
    | ind == 0 = head li
    | otherwise li !! (ind -1)

elem :: Eq a => a -> [a] -> Bool
elem (x:xs)
    | 

merge :: (Ord a) => [a] -> [a] -> [a]
merge = undefined

half :: [a] -> ([a],[a])
half = undefined

msort :: (Ord a) => [a] -> [a]
msort = undefined

tokenise :: String -> [String]
tokenise = undefined
