--oppg1
{-
Ble litt usikker, hva med en halvlat versjon? innvendig -> utvendig -> repeat
double (double 2)
double (2+2)
(2+2)+(2+2)
4+4
8
utv -> innv -> repeat
double (double 2)
double 2 + double 2
(2+2) + (2+2)
4 + 4
8

rar oppgave - har jeg misforstått
-}

--oppg2

{- mellomliggende =
sum [x] 
x + sum []
x + 0 //sum [] = 0 pr definisjon av sum s.9 i programming with haskell
x
-}

--oppg3

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct  (x:xs) = x * myProduct xs

{-
product [2,3,4]
2 * product [3,4]
2 * 3 * product [4]
2 * 3 * 4 * product []
2 * 3 * 4 * 1
6 * 4 * 1 = 24
-}

--oppg4

revQS :: Ord a => [a] -> [a]
revQS (x:[]) = [x]
revQS [] = []
revQS (x:xs) = revQS [y|y<-xs,y>x] ++ [x] ++ revQS [y|y<-xs,y<=x]

--
{-
The effect is that of removing duplicate values
-}