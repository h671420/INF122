--oppg1 ok

--oppg2

main :: IO ()
main = putStrLn $
    "2^3*4 = (2^3)*4 = 32 = " ++ show c1 ++ "\n"++
    "2*3 + 4*5 = (2*3) + (4*5) = 26 = " ++ show c2 ++ "\n"++
    "2 + 3 * 4^5 = 2 + (3 * (4^5)) = 3074 = " ++ show c3
        where 
            c1 = 2^3*4
            c2 = 2*3+4*5
            c3 = 2+3*4^5

--oppg3
{-
N = a 'div' length xs
    where
         a = 10
        xs = [1,2,3,4,5]

capitalized function name N, indentation error & retarded '' != `` rule as in javascript. fucking bullshit man...
-}

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

--oppg4
var = [4,5,7,2]

myLast1 :: [a]->a
myLast1 xs = head (drop (length xs -1) xs)

myLast2 :: [a]->a
myLast2 xs = head (reverse xs)

--oppg5
init2 xs = take (length xs -1) xs 

init3 xs = reverse (tail (reverse xs))
