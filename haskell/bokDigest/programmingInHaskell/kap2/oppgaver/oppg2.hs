--OK
main :: IO ()
main = putStrLn $
    "2^3*4 = (2^3)*4 = 32 = " ++ show c1 ++ "\n"++
    "2*3 + 4*5 = (2*3) + (4*5) = 26 = " ++ show c2 ++ "\n"++
    "2 + 3 * 4^5 = 2 + (3 * (4^5)) = 3074 = " ++ show c3
        where 
            c1 = 2^3*4
            c2 = 2*3+4*5
            c3 = 2+3*4^5
