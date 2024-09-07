--oppg1
{-
1+(2*3)
    outer red = 1 + (2*3)
    inner red = 2 * 3 

(1+2) * (2+3)
    outer red =  .. * ..
    inner red 1 = 1+2
    inner red 2 = 2+3

fst (1+2,2+3)
    outer red = fst (..,..)
    inner red 1 = 1+2
    inner red = 2+3

(\x -> 1+x) (2*3)
    outer red = (\x -> 1+x)
    inner red = 2*3
-}

--oppg2
{-

fst (1+2,2+3)

outermost eval -> 
        fst (1+2,2+3)
        1+2
        3
innermost
        fst (1+2,2+3)
        fst (3,2+3)
        fst (3,5)
        3

outermost saves time by not evaluating the second touplemember.
-}

--oppg3
{-
mult = \x -> (\y -> x*y)
mult 3 4
4 -> (\y -> 3*y) 
3*4
12
-}

-oppg4
-- min, ikke så effektiv
fibs = [internalfib x| x<-[0..]] where
    internalfib 0 = 0
    internalfib 1 = 1
    internalfib n = internalfib (n-1) + internalfib (n-2)

-- CGPT sin, skjønner ikke meg på den
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

