--oppg1
    halve :: [a] -> ([a],[a])
    halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

--oppg2  
    third1,third2,third3 :: [a] -> a
    third1 xs = head (tail (tail xs))
    third2 xs = xs !! 2
    third3 (a:b:c:xs) = c

--oppg3
    safetail1,safetail2,safetail3 :: [a]->[a]    
    safetail1 list = if null list then [] else tail list
    safetail2 list  | null list = []
                    | otherwise = tail list
    safetail3 [] = []
    safetail3 list = tail list

--oppg4
    (||*) :: Bool -> Bool -> Bool
    (||**) :: Bool -> Bool -> Bool
    (||***) :: Bool -> Bool -> Bool
    (||****) :: Bool -> Bool -> Bool

    False ||* False = False
    False ||* True = True
    True ||* False = True
    True ||* True = True

    False ||** False = False
    _ ||** _ = True

    True ||*** _ = True
    _ ||*** True = True
    _ ||*** _ = False

    a ||**** b  | a == b = a
                | otherwise = True

--oppg5
    (&&*) :: Bool -> Bool -> Bool
    a &&* b = 
        if a == True
            then if b == True   
                then True
            else False
        else if b == True
            then False
            else False

    (&&**) :: Bool -> Bool -> Bool
    a &&** b = if a == b then a else False
    
--7
    mult :: Int -> Int -> Int -> Int
    mult = \x -> \y -> \z -> x*y*z
    
    luhnDouble :: Int -> Int
    luhnDouble a    | a>=0 && a<5 = a*2
                    | a >=5 && a < 10 = 2*a -9

    luhn4Digits :: Int -> Int -> Int -> Int -> Bool
    luhn4Digits a b c d = (luhnDouble a + b + luhnDouble c + d ) `mod` 10 == 0