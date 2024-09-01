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