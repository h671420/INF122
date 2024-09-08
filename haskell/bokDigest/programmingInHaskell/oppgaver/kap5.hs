--oppg1
    sum100sq = sum [x*x|x<-[1..100]]

--oppg2
    grid :: Int -> Int -> [(Int,Int)]
    grid x y = [(x1,y1)|x1<-[0..x],y1<-[0..y]]

--oppg3
    square :: Int -> [(Int,Int)]
    square s = [c | c <- grid s s ,fst c /= snd c]

--oppg4
    repl :: Int -> a -> [a]
    repl num ele = [ele | _ <- [1..num]]

--oppg5
    pyths :: Int -> [(Int,Int,Int)]
    pyths n = [(a,b,c)|a<-[1..n],b<-[a..n],c<-[b..n],a*a + b*b == c*c]

--oppg6
    factors :: Int -> [Int]
    factors n = [x|x<-[1..n],n `mod`x==0]
    perfects :: Int ->[Int]
    perfects n = [x | x <-[1..n],x*2 == sum (factors x)] 

--oppg7
    --skipp

--oppg8
    --skipp

--oppg9
    scalarproduct :: [Int]->[Int]->Int
    scalarproduct li1 li2 = sum[el1*el2| (el1,el2)<-zip li1 li2]

--oppg10
    --skipp