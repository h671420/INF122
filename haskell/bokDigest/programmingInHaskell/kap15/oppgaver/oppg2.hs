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