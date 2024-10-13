module Oblig1 where

import Data.Char

--spm1
tokenise :: String -> [String]
tokenise str 
    | null str = []
    | head str == '*' = [head str]:tokenise (tail str)
    | head str == '/' = [head str]:tokenise (tail str)
    | head str == '+' = [head str]:tokenise (tail str)
    | head str == '-' = [head str]:tokenise (tail str)
    | head str == '(' = [head str]:tokenise (tail str)
    | head str == ')' = [head str]:tokenise (tail str)
    | isDigit (head str) = takeWhile isDigit str : tokenise (dropWhile isDigit str) 
    | isLetter (head str) = takeWhile isDigit str : tokenise (dropWhile isDigit str) 
    | otherwise = tokenise (tail str)
    
data Op = Add | Sub | Mult | Div deriving (Eq, Show)
--data Ast = BinOp Op Ast Ast | Tall Int deriving (Eq, Show)
data Ast = BinOp Op Ast Ast | Tall Int | Var String deriving (Eq, Show) 

fromToken :: String -> Op
fromToken token 
    | token == "+" = Add
    | token == "-" = Sub
    | token == "*" = Mult
    | token == "/" = Div


--spm2
parseTerm :: [String] -> (Ast , [String])
parseTerm (token:tokens)
    | and [isNumber c|c<-token] = (Tall (read token::Int) , tokens)


--spm3
parseFactor :: [String] -> (Ast , [String])
parseFactor (x:[]) = (fst (parseTerm [x]),[])
parseFactor (te:op:rem) = (BinOp (fromToken op) (Tall (read te::Int)) (fst (parseFactor rem)),[])


--spm4
{--
parseExpr :: [String] -> (Ast , [String])
parseExpr tokens
    | (getLeft tokens) == tokens = parseFactor tokens
    | otherwise = (BinOp (fromToken (getOp tokens)) (fst (parseFactor (getLeft tokens))) (fst (parseExpr (getRight tokens))),[])
         where  
            getLeft :: [String] -> [String]
            getLeft tokens= takeWhile (\x -> x /="-" && x /="+") tokens
            getOp :: [String] -> String
            getOp tokens = head (dropWhile (\x -> x /="-" && x /="+") tokens)
            getRight :: [String] -> [String]
            getRight tokens = tail (dropWhile (\x -> x /="-" && x /="+") tokens)
--}
parseExpr :: [String] -> (Ast , [String])
parseExpr tokens
    | (getLeft tokens) == tokens = parseFactor tokens
    | otherwise = (BinOp (fromToken (getOp tokens)) (fst (parseFactor (getLeft tokens))) (fst (parseExpr (getRight tokens))),[])
         where  
            getLeft :: [String] -> [String]
            getLeft = getLeft' 0
                where
                    getLeft' :: Int -> [String] -> [String]
                    getLeft' _ [] = []  -- End of tokens
                    getLeft' 0 (x:xs)
                        | x == "-" || x == "+" = []  
                        | otherwise = x : getLeft' 0 xs
                    getLeft' level (x:xs)
                        | x == "(" = x : getLeft' (level + 1) xs  
                        | x == ")" = x : getLeft' (level - 1) xs  
                        | otherwise = x : getLeft' level xs  

            getOp :: [String] -> String
            getOp = getOp' 0
                where
                    getOp' :: Int -> [String] -> String
                    getOp' _ [] = error "No operator found"  -- Error case when no operator is found
                    getOp' 0 (x:xs)
                        | x == "-" || x == "+" = x  -- Return the first `-` or `+` at base level
                        | otherwise = getOp' 0 xs
                    getOp' level (x:xs)
                        | x == "(" = getOp' (level + 1) xs  -- Increment level inside parentheses
                        | x == ")" = getOp' (level - 1) xs  -- Decrement level when leaving parentheses
                        | otherwise = getOp' level xs

            getRight :: [String] -> [String]
            getRight = getRight' 0
                where
                    getRight' :: Int -> [String] -> [String]
                    getRight' _ [] = []  -- No more tokens to process
                    getRight' 0 (x:xs)
                        | x == "-" || x == "+" = xs  -- Skip the first `-` or `+` at base level, and return the rest
                        | otherwise = getRight' 0 xs
                    getRight' level (x:xs)
                        | x == "(" = getRight' (level + 1) xs  -- Increment level inside parentheses
                        | x == ")" = getRight' (level - 1) xs  -- Decrement level when leaving parentheses
                        | otherwise = getRight' level xs


--spm5

--spm6
parse :: String -> Ast
parse inp = fst( parseExpr (tokenise inp))


--spm7
eval :: Ast -> Int
eval (Tall t) = t
eval (BinOp op v h )
    | op == Add = (eval v) + (eval h)
    | op == Sub = (eval v) - (eval h)
    | op == Mult = (eval v) * (eval h)
    | op == Div = (eval v) `div` (eval h)

--spm8
ppInfix :: Ast -> String
ppInfix (Tall t) = show t
ppInfix (BinOp op v h )
    | op == Add = (ppInfix v) ++ " + " ++(ppInfix h)
    | op == Sub = (ppInfix v) ++ " - " ++ (ppInfix h)
    | op == Mult = (ppInfix v) ++ " * " ++ (ppInfix h)
    | op == Div = (ppInfix v) ++ " / " ++ (ppInfix h)
 
 --spm9
ppPN :: Ast -> String 
ppPN (Tall t) = show t
ppPN (BinOp op v h )
    | op == Add = "+ " ++ (ppPN v) ++ ' ':(ppPN h)
    | op == Sub = "- " ++ (ppPN v) ++ ' ':(ppPN h)
    | op == Mult = "* " ++ (ppPN v) ++ ' ':(ppPN h)
    | op == Div = "/ " ++ (ppPN v) ++ ' ':(ppPN h)
 
 
 --spm10
ppOPN :: Ast -> String
ppOPN (Tall t) = show t
ppOPN (BinOp op v h )
    | op == Add = (ppOPN v) ++ ' ':(ppOPN h) ++" +" 
    | op == Sub = (ppOPN v) ++ ' ':(ppOPN h) ++" -" 
    | op == Mult = (ppOPN v) ++ ' ':(ppOPN h) ++" *" 
    | op == Div = (ppOPN v) ++ ' ':(ppOPN h) ++" /" 













