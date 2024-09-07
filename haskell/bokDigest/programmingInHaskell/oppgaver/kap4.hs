--oppg1
    halve :: [a] -> ([a],[a])
    halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

-oppg2  
    third1,third2,third3 :: [a] -> a
    third1 xs = head (tail (tail xs))
    third2 xs = xs !! 2
    third3 (a:b:c:xs) = c

-oppg3
    safetail1,safetail2,safetail3 [a]->[a]    
    safetail1 list = if null list then [] else tail list
    safetail2 list  | null list = []
                    | otherwise tail list
    safetail3 [] = []
    safetail3 list = tail list

(||1),(||2),(||3),(||4) :: Bool -> Bool -> Bool

(||1) False False = False
(||1) False True = True
(||1) True False = True
(||1) True True = True

(||2) False False = False
(||2) _ _ = True

(||3) False other = other
(||3) False _ = False
