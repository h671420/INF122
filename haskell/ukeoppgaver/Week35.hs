module Week35 where

double :: [Int] -> [Int]
double (x:xs) = (x*2):double xs
double [] = []

sm1, sm2 :: [Int] -> Int
sm1 (x:xs) = x + sm1 xs
sm1 [] = 0

sm2 xs = sum xs

adds :: [(Int,Int)] -> [Int]
adds (pair:pairs) = (fst pair + snd pair):adds pairs
adds [] = []
{--}
q1 :: Int -> Int
q1 = (+) 2

q2 :: Int -> Int 
q2 = (+ 2)

q3 :: Int -> Int 
q3 = (2 +)

q4 :: ([String],Char)
q4 = (["foo", "bar"], 'a')

--q5 :: undefined - "foo" og 'a' er ikke av samme type og kan ikke være i samme liste
--q5 = (["foo", 'a'], "bar")

q6 :: [(Bool, [String])]
q6 = [(True, []), (False, [['a']])]


q7 :: Int -> [b] -> b
q7 = \x y -> y !! x

q8 :: [Int -> [a] -> [a]]
q8 = [ take, drop, \x y -> [y !! x] ]

