import NumberNames (spellInteger)
import qualified Data.Text.Lazy as T
import Data.Char

spellOrdinal :: Integral i => i -> String
spellOrdinal n
 | n <=   0  = "not ordinal"
 | n <   20  = small n
 | n < 100   = case divMod n 10 of
     (k, 0) -> spellInteger (10*k) ++ "th"
     (k, m) -> spellInteger (10*k) ++ "-" ++ spellOrdinal m
 | n < 1000 = case divMod n 100 of
     (k, 0) -> spellInteger (100*k) ++ "th"
     (k, m) -> spellInteger (100*k) ++ ", " ++ spellOrdinal m
 | otherwise = case divMod n 1000 of
     (k, 0) -> spellInteger (1000*k) ++ "th"
     (k, m) -> spellInteger (k*1000) ++ ", " ++ spellOrdinal m
 where 
   small = ([ undefined, "first", "second", "third", "fourth", "fifth"
            , "sixth", "seventh", "eighth", "nineth", "tenth", "eleventh"
            , "twelveth", "thirteenth", "fourteenth", "fifteenth", "sixteenth"
            , "seventeenth", "eighteenth", "nineteenth"] !!) . fromEnum


sentence = start ++ foldMap add (zip [2..] $ tail $ words sentence)
  where
    start = "Four is the number of letters in the first word of this sentence, "
    add (i, w) = unwords [spellInteger (alphaLength w), "in the", spellOrdinal i ++ ", "]

alphaLength w = fromIntegral $ length $ filter isAlpha w

main = mapM_ (putStrLn . say) [1000,10000,100000,1000000]
  where
    ws = words sentence
    say n =
      let (a, w:_) = splitAt (n-1) ws
      in "the " ++ spellOrdinal n ++ " word is \"" ++ w ++ "\" which has " ++
         spellInteger (alphaLength  w) ++ " letters. The sentence length is " ++
         show (length $ unwords a) ++ " chars."
