
module Week38 where

remg,remgh :: [a] -> (a -> Bool) -> [a]
remg li pr = takeWhile (not.pr) li ++ drop 1 (dropWhile (not.pr) li)

remgh li pr = remghh li pr [] where
        remghh :: [a] -> (a -> Bool) -> [a] -> [a]
        remghh li pre acc 
            | null li = acc
            | (not.pre) (head li) = remghh (tail li) pre (head li:acc)
            | pre (head li) = reverse acc ++ tail li


mapF :: (a -> b) -> [a] -> [b]
mapF = undefined

{-
counterExample :: [Bool]
counterExample = undefined


-- 7.1
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter = undefined


-- 7.4
dec2int :: [Int] -> Int
dec2int = undefined

-- 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap = undefined

-- 7.10
luhn :: [Int] -> Bool
luhn = undefined
-}