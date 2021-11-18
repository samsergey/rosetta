import Data.List
import Data.Char

listToString :: [Int] -> String
listToString = unwords . map show

stringToInt :: String -> Int
stringToInt s = foldr go 0 . map (\c -> mod (ord c - 48) 13)
  where
    go r x = 13*r + x

charToInt c = mod (ord c - 48) 13

intToChar n = 
  
listToInt lst = 0
