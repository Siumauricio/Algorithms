import System.IO  

import Data.Char
import Data.List
import Data.Maybe


hash :: [Char] -> Int
hash [] = 0
hash (x:y) = (ord x)*7^(length (x:y)-1) + hash y

rabinKarp :: String -> String-> Int -> Int
rabinKarp [] _ _ = 0
rabinKarp mainString patternString k=
    let
     hashPattern = hash patternString
     hashMain = hash (take (length patternString) mainString)
    in if hashPattern == hashMain
    then k + rabinKarp (drop 1 mainString) patternString k+1
    else rabinKarp (drop 1 mainString ) patternString k



main = do
    handle <- openFile "file.txt" ReadMode
    contents <- hGetContents handle
    let var = contents
    print(rabinKarp var "computers" 0)
    hClose handle
