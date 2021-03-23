import System.IO

levenshtein::[Char] -> [Char] -> Int
levenshtein "" "" = 0
levenshtein "" s2 = length s2
levenshtein s1 "" = length s1
levenshtein s1 s2
   | last s1 == last s2 = levenshtein (init s1) (init s2)
   | otherwise = minimum [1 + levenshtein (init s1) s2,
                          1 + levenshtein s1 (init s2),
                          1 + levenshtein (init s1) (init s2)]

comparar :: String -> [String] ->Int-> [String]
comparar _ [] _= []
comparar palabra (x:xs) tamano =  
  let value = levenshtein palabra x 
  in if value >= 1 && length x == tamano && value <=2
  then [x] ++ (comparar palabra xs tamano)
  else (comparar palabra xs tamano)

main = do
  handle <- openFile "file.txt" ReadMode
  contents <- hGetContents handle
  let var = words contents
  let word = "carlos" --AQUI VA EL TEXTO DEL TEXTFIELD
  let value = length word 
  print (comparar word var value)  --RETORNA UNA LISTA CON TODAS LAS STRINGS CASI IGUALES
  hClose handle
