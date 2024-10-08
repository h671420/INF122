--oppg1
    fact :: Int ->Int
    fact 0 = 1
    fact n = n * fact (n-1)
    {-
    The above will never hit a base case if applied to a negative, causing a stackerror 
    The below will instead throw an error
    -}
    safefact :: Int ->Int
    safefact n  | n == 0 = 1
            | n < 0 = error "negatives verboten"
            | otherwise = n * fact (n-1)

--oppg2
    sumdown :: Int ->Int
    sumdown 0 = 0
    sumdown n = n+ sumdown (n-1)

--oppg3
    (^*) :: Int -> Int -> Int
    g^*0 = 1
    g^*e = g * g^*(e-1)

--oppg4
    euclidGCD :: Int -> Int -> Int
    euclidGCD a b   | a == b = a
                    | otherwise = euclidGCD (min a b) (max a b - min a b)

--oppg5
    {-
    length [1,2,3]
    1 + length [2,3]
    1 + 1 + length [3]
    1 + 1 + 1 + length []
    1 + 1 + 1 + 0
    3

    drop 3 [1,2,3,4,5]
    drop 2 [2,3,4,5]
    drop 1 [3,4,5]
    drop 0 [4,5]
    [4,5]

    init [1,2,3]
    1:(init[2,3])
    1:2:(init[3])
    1:2:[])
    -}

--oppg6
    myAnd :: [Bool] -> Bool
    myAnd [] = True
    myAnd (x:xs) = x && myAnd xs

    myConcat :: [[a]]->[a]
    myConcat ([]:xss) = myConcat xss
    myConcat ((x:xs):xss) = x:myConcat (xs:xss)
    
    myRepl :: Int -> a -> [a]
    myRepl 0 val = []
    myRepl i val = val : myRepl (i-1) val

    (!!*) :: [a] -> Int -> a
    list !!* 0 = head list
    list !!* n = tail list !!* (n-1)

    findelem :: Eq a => a -> [a] -> Bool
    findelem val (x:xs) | x == val = True
                    | otherwise = findelem val xs

--oppg7
    merge :: Ord a => [a] -> [a] -> [a]
    merge a [] = a
    merge [] a = a
    merge (a:as) (b:bs) | a<b = a: merge as (b:bs)
                        |otherwise = b: merge (a:as) bs
    
    halve :: [a]->([a],[a])
    halve li = splitAt (div (length li) 2) li
    
    msort :: Ord a => [a] -> [a]
    msort li    | length li <2 = li
                | otherwise = merge (msort (fst (halve li))) (msort (snd (halve li)))
    
    li = [5,6,8,2,1,7]