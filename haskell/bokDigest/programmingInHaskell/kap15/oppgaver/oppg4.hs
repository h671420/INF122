-- min, ikke så effektiv
fibs = [internalfib x| x<-[0..]] where
    internalfib 0 = 0
    internalfib 1 = 1
    internalfib n = internalfib (n-1) + internalfib (n-2)

-- CGPT sin, skjønner ikke meg på den
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

