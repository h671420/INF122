{-# LANGUAGE BangPatterns, CPP #-}
module Oblig2 where

import Data.Char
import Data.List
import System.IO

#if ! MIN_VERSION_base(4,15,0)
readFile' :: FilePath -> IO String
readFile' f = do
  str <- readFile f
  let !_ = length str
  return str
#endif



data Regex = Atom Char       -- Single character
           | After Regex Regex  -- Concatenation (sequence)
           | Both Regex Regex   -- Alternation (either one or the other)
           | Kleene Regex       -- Kleene star (zero or more repetitions)
           | Empty              -- Empty regex
           | Any                -- Matches any character (.)
  deriving (Show, Eq)

-- Parsing a complete regex, handling sequences of `Re1`
reg :: String -> (Regex, String)
reg "" = (Empty, "")
reg input =
    let (firstRe1, rest) = re1 input
    in case rest of
        "" -> (firstRe1, "")
        _  -> let (nextReg, remaining) = reg rest
              in (After firstRe1 nextReg, remaining)  -- Concatenation in sequence

-- Parsing `Re1`, which checks for Kleene star on `Re2`
re1 :: String -> (Regex, String)
re1 input =
    let (parsedRe2, rest) = re2 input
    in case rest of
        ('*':xs) -> (Kleene parsedRe2, xs)  -- Kleene star applied
        _ -> (parsedRe2, rest)

-- Parsing `Re2`, which checks for alternation (|)
re2 :: String -> (Regex, String)
re2 input =
    let (parsedRe3, rest) = re3 input
    in case rest of
        ('|':xs) -> 
            let (nextRe2, remaining) = re2 xs
            in (Both parsedRe3 nextRe2, remaining)  -- Alternation (either-or)
        _ -> (parsedRe3, rest)

-- Parsing `Re3`, which handles individual atoms, dots, and groups in parentheses
re3 :: String -> (Regex, String)
re3 (c:cs)
    | c == '.' = (Any, cs)  -- `.` matches any character
    | c == '(' = 
        let (insideGroup, rest) = reg cs
        in case rest of
            (')':xs) -> (insideGroup, xs)  -- Match and close parentheses
            _ -> error "Unmatched parenthesis"
    | otherwise = (Atom c, cs)  -- Single character atom
re3 [] = (Empty, [])  -- Empty input returns `Empty`




{--  
re1 inp
    | (getLeft inp operators) == inp = re2 inp
    | otherwise = (Kleene (fst (re2 (getLeft inp operators))) ,[])
         where  
            operators = "*"


re2 inp
    | (getLeft inp operators) == inp = re3 inp
    | otherwise = (Both (fst (re3 (getLeft inp operators))) (fst (reg (getRight inp operators))) ,[])
         where  
            operators = "|"

re3 inp
  | (head inp) == '.' = (Any ,[])
  | (head inp) == '(' = reg (reverse(tail(reverse(tail inp))))
  | otherwise = (Atom (head inp),[])
--}

type Transition = String -> [(String,String)]

matchChar :: Char -> Transition
matchChar = undefined

matchAny :: Transition
matchAny = undefined

matchEmpty :: Transition
matchEmpty = undefined

matchBoth :: Transition -> Transition -> Transition
matchBoth = undefined

matchAfter :: Transition -> Transition -> Transition
matchAfter = undefined

regex2trans :: Regex -> Transition
regex2trans = undefined

longest :: [String] -> String
longest = undefined

matchStart :: String -> String -> String
matchStart = undefined

matchLine :: String -> String -> String
matchLine = undefined

replaceLine :: String -> (String -> String) -> String -> String
replaceLine = undefined

grep :: String -> FilePath -> IO ()
grep = undefined

sed :: String -> (String -> String) -> FilePath -> FilePath -> IO ()
sed = undefined

{--
getLeft :: String -> String-> String
getLeft symbols operators = getLeft' 0 symbols operators
    where
        getLeft' :: Int -> String -> String -> String
        getLeft' _ [] _ = [] 
        getLeft' level (x:xs) operators
            | x == '(' = x : getLeft' (level + 1) xs operators
            | x == ')' = x : getLeft' (level - 1) xs operators
            | (x `isIn` operators) && level == 0 = []  
            | otherwise = x : getLeft' level xs operators


getLeft :: String -> String-> String
getLeft tokens operators = getLeft' 0 tokens operators
    where
        getLeft' :: Int -> String -> String -> String
        getLeft' _ [] _ = [] 
        getLeft' level (x:xs) operators
            | x == '(' = x : getLeft' (level + 1) xs operators
            | x == ')' = x : getLeft' (level - 1) xs operators
            | (x `isIn` operators) && level == 0 = []  
            | otherwise = x : getLeft' level xs operators

getRest :: String -> String-> String
getRest tokens operators = getRest' 0 tokens operators
    where
        getRest' :: Int -> String -> String-> String
        getRest' _ [] _ = [] 
        getRest' level (x:xs) operators
            | x == '(' = getRest' (level + 1) xs operators
            | x == ')' = getRest' (level - 1) xs operators
            | (x `isIn` operators) && level == 0 = x:xs
            | otherwise = getRest' level xs operators

getOp :: String -> String-> Char
getOp tokens operators = head (getRest tokens operators)

getRight :: String -> String-> String
getRight tokens operators = tail (getRest tokens operators)
--}
isIn :: Char -> String -> Bool
isIn string [] = False
isIn string (head:tail) = string == head || isIn string tail


{--
'(' '_' -> pass

--}

getParanthesis :: String -> (String , String)
getParanthesis string = split' ([],string) 0
  where
    split' :: (String, String) -> Int -> (String , String)
    split' (out, input) level 
      | not (null input) && (head input) == '(' = split' (head input:out, tail input) (level + 1) 
      | not (null input) && (head input) == ')' = split' (head input:out, tail input) (level - 1)  
      | level == 0 = (reverse(out), input)
      | otherwise = split' (head input:out, tail input) level