
oppg1a  :: String
oppg1a = ['a','b','c']

oppg1b :: (Char,Char, Char)
oppg1b = ('a','b','c')

oppg1c ::[(Bool,Char)]
oppg1c = [(False,'0'),(True,'1')]

oppg1d :: ([Bool],[Char])
oppg1d = ([False,True],['0','1'])

oppg1e :: [[a]->[a]]
oppg1e = [tail,init,reverse]

--oppg2
bools ::[Bool]
bools = [False,True]

nums ::[[Int]]
nums = [[1,2],[3,4],[4,5]]

add :: Int -> Int -> Int -> Int
add a b c = 1

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a->b) -> a -> b
apply funksjon input = funksjon input

--oppg3
second :: [a]->a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair a b = (a,b)

double:: Num x => x ->x
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a->a) -> a -> a   
twice f x = f (f x)

--Oppg4 OK

