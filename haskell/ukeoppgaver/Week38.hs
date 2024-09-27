
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
mapF f li = foldr ((:).f) [] li

and1, and2 :: [Bool] -> Bool
and1 = foldr (&&) True
and2 = foldl (&&) True
trues = [True|x<-[1..]]
liez = [False|x<-[1..]]

counterExample :: [Bool]
counterExample = undefined





-- 7.1
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p li= foldr (\x xs->if (p x) then f x : xs else xs ) [] li


-- 7.4
dec2int :: [Int] -> Int
dec2int = foldl (\acc head -> acc*10 + head) 0 


-- 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 li = foldr (\(i,v) li -> if (even i) then (f1 v):li else (f2 v):li) [] (zip [0..] li)


-- 7.10
luhn :: [Int] -> Bool
luhn li = (sum(altMap id luhnDouble (reverse li)))`mod`10==0 where
    luhnDouble::Int -> Int
    luhnDouble x 
        | x*2 >9 = x*2 - 9
        | otherwise = x*2
{--}
 
asd = [7,6,0,0,4,6,2,8,8,2,4,9,3,5,5,5]
