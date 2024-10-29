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


data Regex = Atom Char | Both Regex Regex | After Regex Regex | Kleene Regex | Empty | Any
  deriving (Show, Eq)

reg, re1, re2, re3 :: String -> (Regex, String)

reg input
  | null input = (Empty, input)
  | otherwise = 
    case rest of 
      "" -> (reg1, rest)
      _ -> (After reg1 reg2, remaining) 
  where
    (reg1, rest) = re1 input
    (reg2, remaining) = reg rest

re1 input =
  case rest of 
    ('*':xs) -> (Kleene reg1, xs)
    _ -> (reg1, rest)
  where
    (reg1, rest) = re2 input

re2 input =
  case rest of 
    ('|':xs) -> (Both reg1 reg2, remainder)
    _ -> (reg1, rest)
  where
    (reg1, rest) = re3 input
    (reg2, remainder) = re3 (tail rest)
  
re3 (c:cs) 
  |c == '.' = (Any, cs)
  |c == '(' = (fst (reg (fst (getExpr (c:cs)))), snd (getExpr (c:cs)))
  |otherwise = (Atom c, cs)

getExpr :: String -> (String, String)
getExpr string = (reverse (tail (reverse (tail (fst (split' ([],string) 0))))), snd (split' ([],string) 0))  
  where
    split' :: (String, String) -> Int -> (String , String)
    split' (out, input) level 
      | not (null input) && (head input) == '(' && level >=0  = split' (head input:out, tail input) (level + 1) 
      | not (null input) && (head input) == ')' && level == 1 = split' (head input:out, tail input) (level - 2)  
      | not (null input) && (head input) == ')' = split' (head input:out, tail input) (level - 1)  
      | level <= 0 = (reverse(out), input)
      | otherwise = split' (head input:out, tail input) level

type Transition = String -> [(String,String)]

matchChar :: Char -> Transition
matchChar c = mc c where
  mc :: Char -> String -> [(String, String)]
  mc c str
    | null str = []
    | c == head str = [([c], tail str)]
    | otherwise = []

matchAny :: Transition
matchAny = split where
  split :: String -> [(String, String)]
  split str 
    |null str = []
    |otherwise = [(take 1 str, tail str)]

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
