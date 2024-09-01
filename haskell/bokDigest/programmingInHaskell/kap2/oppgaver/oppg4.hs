--OK
var = [4,5,7,2]

myLast1 :: [a]->a
myLast1 xs = head (drop (length xs -1) xs)

myLast2 :: [a]->a
myLast2 xs = head (reverse xs)