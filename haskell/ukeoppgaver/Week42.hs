{-# LANGUAGE CPP, BangPatterns #-}
module Main where

import Data.List
import System.IO

{- Message from James:

  Firstly, you do not need to understand this! You can ignore the wird things in this file. But if you want to
  understand it, I have attached an explanation.

  Code grade has a really old version of haskell which does not have readFile'
  I have implemented a version of it such that it will only be defined if your haskell version is as old as code grade's:
    - Line 1 is turning on some language features.
    - CPP let's me use the PreProcessor
    - BangPatterns let's me add '!' is clever places to control laziness
    - Line _ is using the preprocessor to say "only keep the following code if ghc's library is older than 4.15.0"
    - line _ uses the bang (!). It makes the function depend on the let expression:
        - length forces str to be fully evaluated
        - let !_ forces my readLine' function to depend on length
      and so together, it forces haskell to read the entire file and close it before we get to return str.
-}

#if ! MIN_VERSION_base(4,15,0)
readFile' :: FilePath -> IO String
readFile' f = do
  str <- readFile f
  let !_ = length str
  return str
#endif


-- Tip 1: You will definitly want to define a bunch of function to complete this task. Not just implement main.
-- Tip 2: If you want to run your main function from ghci, type :main

main :: IO ()
main = do
  --putStrLn ""
  loop True 

loop :: Bool -> IO ()
loop running = do
  if running
    then do
      --visMeny  
      input <- getLine
      let wordlist = words input
      case wordlist of
        ("hei":resten) -> reverserReturner wordlist
        ("vis":resten) -> skrivFil wordlist
        ("bytt":resten) -> bytt wordlist 
        ("av":resten) -> avslutt
        otherwise -> ugyldigInput input
    else return ()

visMeny :: IO ()
visMeny = do
  
  putStrLn "Vennligst velg en av følgende:"
  putStrLn "\t 'hei xyz' får programmet til å skrive ut xyz argumentet reversert"
  putStrLn "\t 'vis navn' viser innholdet av filen med 'navn' på skjermen"
  putStrLn "\t 'bytt aa bb navn' bytter hver forekomst av strengen 'aa' til 'bb' i filen med navnet 'navn'"
  putStrLn "\t 'av' avslutter interaksjonen og terminerer programmet"

reverserReturner :: [String] -> IO()
reverserReturner wordlist = do
  if length wordlist > 1 then
    putStrLn (reverse $ wordlist!!1)
  else
    putStrLn ""
  --getLine
  loop True 

skrivFil :: [String] -> IO()
skrivFil wordlist = do 
  if length wordlist > 1 then do
    content <- readFile (wordlist!!1)
    putStrLn  content
  else
    putStrLn "no filename given.."
  --getLine
  loop True 

bytt :: [String] -> IO()
bytt wordlist = do
  if length wordlist > 3 then do
    content <- readFile (wordlist!!3)
    if (length content >0 ) then
      writeFile (wordlist!!3) (replace (wordlist!!1) (wordlist!!2) content)
    else return ()
    --putStrLn (replace (wordlist!!1) (wordlist!!2) content)
  else
    putStrLn "no filename given.."
  --getLine
  loop True 

avslutt :: IO()
avslutt = return ()
{--
avslutt = do
  putStrLn "Programmet avsluttes"
  getLine
  return ()
  --}

ugyldigInput :: String -> IO()
ugyldigInput input = do
  putStrLn("'"++input++"' er ugyldig input - trykk enter for å fortsette")
  getLine
  loop True 

replace :: String -> String -> String -> String
replace aa bb text
  | length aa > length text = text
  | (take (length aa) text) == aa = bb ++  replace aa bb (drop (length aa) text)
  | otherwise = [head text] ++ replace aa bb (tail text)