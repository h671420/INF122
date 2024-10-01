
module Week39 where
import Distribution.Simple.Setup (falseArg)

data Set a = Empty | Singleton a | Union (Set a) (Set a)
    deriving (Eq, Show)

fromList :: [a] -> Set a
fromList a
    | null a = Empty
    | null (tail a) = Singleton (head a)
    | otherwise = Union (Singleton (head a)) (fromList (tail a))


isIn :: Eq a => a -> Set a -> Bool
isIn a Empty = False
isIn a (Singleton b) = a==b
isIn a (Union b c ) = isIn a b || isIn a c


subset :: Eq a => Set a -> Set a -> Bool

subset Empty _ = True
subset (Singleton a) b = isIn a b
subset (Union a c) b = subset a b && subset c b

setEq :: Eq a => Set a -> Set a -> Bool
setEq a b =  subset a b && subset b a



data Nat = Zero | Succ Nat
    deriving (Show, Eq)


foldNat :: a -> (a -> a) -> Nat -> a
foldNat acc _ Zero = acc
foldNat acc funk (Succ nat) = foldNat (funk acc) funk nat

foldSet :: b -> (a -> b) -> (b -> b -> b) -> Set a -> b
foldSet acc _ _ Empty = acc
foldSet _ fc2 _ (Singleton b) = fc2 b
foldSet acc fc2 fc3 (Union a b) = fc3 (foldSet acc fc2 fc3 a) (foldSet acc fc2 fc3 b)


isInF :: Eq a => a -> Set a -> Bool
isInF a = foldSet False (a==) (||)

subsetF :: Eq a => Set a -> Set a -> Bool
subsetF set1 set2 = foldSet True (`isInF` set2) (&&) set1


{--

hvorfor funker den ene men ikke den andre? waah
subset = subsetF

subset Empty _ = True
subset (Singleton a) b = isIn a b
subset (Union (Singleton a) c) b = subset a b && subset c b


foldSet acc _ _ Empty = acc
foldSet _ fc2 _ (Singleton b) = fc2 b
foldSet acc fc2 fc3 (Union a b) = fc3 (foldSet acc fc2 fc3 a) (foldSet acc fc2 fc3 b)

subsetF set1 set2 = foldSet True (`isInF` set2) (&&) set1
--}