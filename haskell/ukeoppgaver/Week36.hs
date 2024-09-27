
module Week36 where

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = xs == reverse xs

third1, third2, third3 :: [a] -> a

third1 xs =head (tail (tail xs))

third2 xs = xs !! 2

third3 (a:b:c:xs) = c

squares1, squares2 :: Int -> Int

squares1 x = sum [x2*x2|x2<-[1..x]]

squares2 = \x -> if x>0 then x*x + squares2 (x-1) else 0
--squares2 0 = 0
--squares2 x = x*x + squares2 (x-1)

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a,b,c)|a<-[1..n],b<-[1..n],c<-[1..n], a*a +b*b == c*c]

luhnDouble :: Int -> Int
luhnDouble i = if i>4 then i*2 -9 else i*2

luhnFixed :: Int -> Int -> Int -> Int -> Bool
luhnFixed a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

luhn :: [Int] -> Bool
luhn xs = sum [if (even i) then n else luhnDouble n | (n,i) <- zip (reverse xs) [0..] ] `mod` 10 == 0
