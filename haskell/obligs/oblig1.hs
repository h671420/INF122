module Oblig1 where

import Data.Char

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

data Op = Add | Sub | Mult | Div
  deriving (Show, Eq)

data Ast = BinOp Op Ast Ast | Tall Int
  deriving (Show, Eq)

fromToken :: String -> Op
fromToken token 
    | token == "+" = Add
    | token == "-" = Sub
    | token == "*" = Mult
    | token == "/" = Div

parseExpr :: [String] -> (Ast, [String])
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

parseFactor :: [String] -> (Ast, [String])
parseFactor (x:[]) = (fst (parseTerm [x]),[])
parseFactor (te:op:rem) = (BinOp (fromToken op) (Tall (read te::Int)) (fst (parseFactor rem)),[])

parseTerm :: [String] -> (Ast, [String])
parseTerm (token:tokens)
    | and [isNumber c|c<-token] = (Tall (read token::Int) , tokens)

parse :: String -> Ast
parse inp = fst( parseExpr (tokenise inp))

eval :: Ast -> Int
eval (Tall t) = t
eval (BinOp op v h )
    | op == Add = (eval v) + (eval h)
    | op == Sub = (eval v) - (eval h)
    | op == Mult = (eval v) * (eval h)
    | op == Div = (eval v) `div` (eval h)

ppInfix :: Ast -> String
ppInfix (Tall t) = show t
ppInfix (BinOp op v h )
    | op == Add = (ppInfix v) ++ " + " ++(ppInfix h)
    | op == Sub = (ppInfix v) ++ " - " ++ (ppInfix h)
    | op == Mult = (ppInfix v) ++ " * " ++ (ppInfix h)
    | op == Div = (ppInfix v) ++ " / " ++ (ppInfix h)

ppPN :: Ast -> String
ppPN (Tall t) = show t
ppPN (BinOp op v h )
    | op == Add = "+ " ++ (ppPN v) ++ ' ':(ppPN h)
    | op == Sub = "- " ++ (ppPN v) ++ ' ':(ppPN h)
    | op == Mult = "* " ++ (ppPN v) ++ ' ':(ppPN h)
    | op == Div = "/ " ++ (ppPN v) ++ ' ':(ppPN h)

ppOPN :: Ast -> String
ppOPN (Tall t) = show t
ppOPN (BinOp op v h )
    | op == Add = (ppOPN v) ++ ' ':(ppOPN h) ++" +" 
    | op == Sub = (ppOPN v) ++ ' ':(ppOPN h) ++" -" 
    | op == Mult = (ppOPN v) ++ ' ':(ppOPN h) ++" *" 
    | op == Div = (ppOPN v) ++ ' ':(ppOPN h) ++" /" 

findVar :: [(String,Int)] -> String -> Int
findVar = undefined

evalVar :: Ast -> [(String,Int)] -> Int
evalVar = undefined
