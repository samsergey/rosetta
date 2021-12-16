module NumberNames where

import Data.List (intercalate, unfoldr)
 
spellInteger :: Integral i => i -> String
spellInteger n
 | n <    0  = "negative " ++ spellInteger (-n)
 | n <   20  = small n
 | n <  100  = let (a, b) = n `divMod` 10
               in  tens a ++ nonzero '-' b
 | n < 1000  = let (a, b) = n `divMod` 100
               in  small a ++ " hundred" ++ nonzero ' ' b
 | otherwise = intercalate ", " $ map big $ reverse $
               filter ((/= 0) . snd) $ zip [0..] $ unfoldr uff n
 
 where 
       nonzero _ 0 = ""
       nonzero c n = c : spellInteger n
 
       
       uff 0 = Nothing
       uff n = Just $ uncurry (flip (,)) $ n `divMod` 1000
 
       
       small = (["zero", "one", "two", "three", "four", "five",
            "six", "seven", "eight", "nine", "ten", "eleven",
            "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
            "seventeen", "eighteen", "nineteen"] !!) . fromEnum
       tens = ([undefined, undefined, "twenty", "thirty", "forty",
           "fifty", "sixty", "seventy", "eighty", "ninety"] !!) .
           fromEnum
 
     
       big (0, n) = spellInteger n
       big (1, n) = spellInteger n ++ " thousand"
       big (e, n) = spellInteger n ++ ' ' : (l !! e) ++ "illion"
         where l = [undefined, undefined, "m", "b", "tr", "quadr",
                   "quint", "sext", "sept", "oct", "non", "dec"]


spellOrdinal :: Integer -> String
spellOrdinal n
 | n <=   0  = "not ordinal"
 | n <   20  = small n
 | n < 100   = case divMod n 10 of
     (k, 0) -> spellInteger (10*k) ++ "th"
     (k, m) -> spellInteger (10*k) ++ "-" ++ spellOrdinal m
 | n < 1000 = case divMod n 100 of
     (k, 0) -> spellInteger (100*k) ++ "th"
     (k, m) -> spellInteger (100*k) ++ " and " ++ spellOrdinal m
 | otherwise = case divMod n 1000 of
     (k, 0) -> spellInteger (100*k) ++ "th"
     (k, m) -> spellInteger (k*1000) ++ s ++ spellOrdinal m
       where s = if m < 100 then " and " else ", "   
 where 
   small = ([ undefined, "first", "second", "third", "fourth", "fifth"
            , "sixth", "seventh", "eighth", "nineth", "tenth", "eleventh"
            , "twelveth", "thirteenth", "fourteenth", "fifteenth", "sixteenth"
            , "seventeenth", "eighteenth", "nineteenth"] !!) . fromEnum
           
test = mapM_ (\n -> putStrLn $ show n ++ "\t" ++ spellOrdinal n)
  [1, 2, 3, 4, 5, 11, 65, 100, 101, 272, 23456, 8007006005004003]
