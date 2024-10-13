import Data.Char
import Text.Parsec (token)
--del1
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
data Ast = BinOp Op Ast Ast | Tall Int deriving (Eq, Show)

parseTerm :: [String] -> (Ast , [String])
parseTerm (token:tokens) 
    | and [isNumber c|c<-token] = (Tall (read token::Int) , tokens)

parseFactor :: [String] -> (Ast , [String])
    | 
{----}