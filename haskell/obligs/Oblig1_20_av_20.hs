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
    | isLetter (head str) = takeWhile isLetter str : tokenise (dropWhile isLetter str) 
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
    | token == "(" = parseExpr (reverse (drop 1 (reverse tokens)) )
    | and [isNumber c|c<-token] = (Tall (read token::Int) , tokens)
    | and [isLetter c|c<-token] = (Var token , tokens)

--spm3
parseFactor :: [String] -> (Ast , [String])
parseFactor tokens
    | (getLeft tokens) == tokens = parseTerm tokens
    | otherwise = (BinOp (fromToken (getOp tokens)) (fst (parseTerm (getLeft tokens))) (fst (parseFactor (getRight tokens))),[])
         where  
            getLeft :: [String] -> [String]
            getLeft = getLeft' 0
                where
                    getLeft' :: Int -> [String] -> [String]
                    getLeft' _ [] = [] 
                    getLeft' level (x:xs)
                        | x == "(" = x : getLeft' (level + 1) xs  
                        | x == ")" = x : getLeft' (level - 1) xs  
                        | (x == "*" || x == "/") && level == 0 = []  
                        | otherwise = x : getLeft' level xs  

            getOp :: [String] -> String
            getOp = getOp' 0
                where
                    getOp' :: Int -> [String] -> String
                    getOp' _ [] = error "No operator found"  
                    getOp' level (x:xs)
                        | x == "(" = getOp' (level + 1) xs
                        | x == ")" = getOp' (level - 1) xs
                        | (x == "*" || x == "/") && level == 0 = x
                        | otherwise = getOp' level xs

            getRight :: [String] -> [String]
            getRight = getRight' 0
                where
                    getRight' :: Int -> [String] -> [String]
                    getRight' _ [] = []  
                    getRight' level (x:xs)
                        | x == "(" = getRight' (level + 1) xs
                        | x == ")" = getRight' (level - 1) xs
                        | (x == "*" || x == "/") && level == 0 = xs  
                        | otherwise = getRight' level xs

--spm4
parseExpr :: [String] -> (Ast , [String])
parseExpr tokens
    | (getLeft tokens) == tokens = parseFactor tokens
    | otherwise = (BinOp (fromToken (getOp tokens)) (fst (parseFactor (getLeft tokens))) (fst (parseExpr (getRight tokens))),[])
         where  
            getLeft :: [String] -> [String]
            getLeft = getLeft' 0
                where
                    getLeft' :: Int -> [String] -> [String]
                    getLeft' _ [] = [] 
                    getLeft' level (x:xs)
                        | x == "(" = x : getLeft' (level + 1) xs  
                        | x == ")" = x : getLeft' (level - 1) xs  
                        | (x == "-" || x == "+") && level == 0 = []  
                        | otherwise = x : getLeft' level xs  

            getOp :: [String] -> String
            getOp = getOp' 0
                where
                    getOp' :: Int -> [String] -> String
                    getOp' _ [] = error "No operator found"  
                    getOp' level (x:xs)
                        | x == "(" = getOp' (level + 1) xs
                        | x == ")" = getOp' (level - 1) xs
                        | (x == "-" || x == "+") && level == 0 = x
                        | otherwise = getOp' level xs

            getRight :: [String] -> [String]
            getRight = getRight' 0
                where
                    getRight' :: Int -> [String] -> [String]
                    getRight' _ [] = []  
                    getRight' level (x:xs)
                        | x == "(" = getRight' (level + 1) xs
                        | x == ")" = getRight' (level - 1) xs
                        | (x == "-" || x == "+") && level == 0 = xs  
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
ppInfix (Var v) = v
ppInfix (BinOp op v h )
    | op == Add = '(':(ppInfix v) ++ " + " ++(ppInfix h)++")"
    | op == Sub = '(':(ppInfix v) ++ " - " ++ (ppInfix h)++")"
    | op == Mult = '(':(ppInfix v) ++ " * " ++ (ppInfix h)++")"
    | op == Div = '(':(ppInfix v) ++ " / " ++ (ppInfix h)++")"
 
 --spm9
ppPN :: Ast -> String 
ppPN (Tall t) = show t
ppPN (Var v) = v
ppPN (BinOp op v h )
    | op == Add = "+ " ++ (ppPN v) ++ ' ':(ppPN h)
    | op == Sub = "- " ++ (ppPN v) ++ ' ':(ppPN h)
    | op == Mult = "* " ++ (ppPN v) ++ ' ':(ppPN h)
    | op == Div = "/ " ++ (ppPN v) ++ ' ':(ppPN h)
 
 
 --spm10
ppOPN :: Ast -> String
ppOPN (Tall t) = show t
ppOPN (Var v) = v
ppOPN (BinOp op v h )
    | op == Add = (ppOPN v) ++ ' ':(ppOPN h) ++" +" 
    | op == Sub = (ppOPN v) ++ ' ':(ppOPN h) ++" -" 
    | op == Mult = (ppOPN v) ++ ' ':(ppOPN h) ++" *" 
    | op == Div = (ppOPN v) ++ ' ':(ppOPN h) ++" /" 

findVar :: [(String,Int)] -> String -> Int
findVar pairs key = head [value | (key', value)<- pairs, key' == key]

evalVar :: Ast -> [(String,Int)] -> Int
evalVar (Var v) pairs= findVar pairs v
evalVar (Tall t) _= t
evalVar (BinOp op v h ) pairs
    | op == Add = (evalVar v pairs) + (evalVar h pairs)
    | op == Sub = (evalVar v pairs) - (evalVar h pairs)
    | op == Mult = (evalVar v pairs) * (evalVar h pairs)
    | op == Div = (evalVar v pairs) `div` (evalVar h pairs)


tokens = tokenise "(22-1)*2+8*2"



