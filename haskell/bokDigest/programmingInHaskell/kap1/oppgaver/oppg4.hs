--OK
revQS :: Ord a => [a] -> [a]
revQS (x:[]) = [x]
revQS [] = []
revQS (x:xs) = revQS [y|y<-xs,y>x] ++ [x] ++ revQS [y|y<-xs,y<=x]