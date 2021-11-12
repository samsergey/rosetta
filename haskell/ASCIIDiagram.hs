<<<<<<< HEAD
import Data.List (nub)
import Data.List.Split (splitOn)
import Control.Monad (unless)
=======
import Text.ParserCombinators.ReadP
import Control.Monad (guard)

data Field a = Field { fieldName :: String
                     , fieldSize :: Int
                     , fieldValue :: Maybe a}

instance Show a => Show (Field a) where
  show (Field n s a) = case a of
    Nothing -> n ++ "\t" ++ show s
    Just x -> n ++ "\t" ++ show s ++ "\t" ++ show x

newtype Data a = Data { fields :: [Field a] }

instance Show a => Show (Data a) where
  show (Data fs) = "NAME\tSIZE\tVALUE\n" ++ unlines (show <$> fs)


instance Read (Data a) where
  readsPrec _ = readP_to_S parseData 

parseData = do n <- parseHeader
               guard (n `elem` [8,16,32,64])
               Data . concat <$> many1 (parseRow n)
  where
    parseRow n = do
      fs <- char '|' *> many parseField <* char '\n'
      guard $ sum (fieldSize <$> fs) == n
      m <- parseHeader
      guard $ m == n
      return fs
 
    parseHeader = do
      char '+'
      n <- length <$> many1 (string "--+")
      char '\n'
      return n
  
    parseField = do
      s1 <- many (char ' ')
      f <- munch1 $ not . (`elem` " |")
      s2 <- many (char ' ')
      char '|'
      let n = (length (s1 ++ f ++ s2) + 1) `div` 3
      return $ Field f n Nothing
    
-- emulation of reading a stream of bits
readData :: Data a -> [b] -> Data [b]
readData d = Data . go (fields d)
  where
    go fs [] = (\f -> f {fieldValue = Nothing}) <$> fs
    go (f:fs) s =
      let (x, xs) = splitAt (fieldSize f) s in
        if length x == fieldSize f
        then f {fieldValue = Just x} : go fs xs
        else go (f:fs) []
  
>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15

diagram = unlines
  ["+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
  ,"|                      ID                       |"
  ,"+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
  ,"|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |"
  ,"+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
  ,"|                    QDCOUNT                    |"
  ,"+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
  ,"|                    ANCOUNT                    |"
  ,"+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
  ,"|                    NSCOUNT                    |"
  ,"+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
  ,"|                    ARCOUNT                    |"
  ,"+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"]
<<<<<<< HEAD

data Field a = Field { fieldName :: String
                     , fieldSize :: Int
                     , fieldValue :: Maybe a}
 
instance Show a => Show (Field a) where
  show (Field n s a) = case a of
    Nothing -> n ++ "\t" ++ show s
    Just x -> n ++ "\t" ++ show s ++ "\t" ++ show x
 
newtype Data a = Data { fields :: [Field a] }
 
instance Show a => Show (Data a) where
  show (Data fs) = "\nNAME\tSIZE\tVALUE\n" ++ unlines (show <$> fs)

readData :: String -> Either String (Data a)
readData d = process $ lines d
  where
    process d = do
      let l = length (head d)
      unless (all ((l ==) . length) d) $
        Left "Table is not aligned!"
      w <- readHLine (head d)
      let rows = filter ((/= "+-") . nub) d
      Data . concat <$> traverse (readRow w) rows
                   
    readHLine s = do
      let cols = splitOn "--" s
      unless (nub cols == ["+"]) $
        Left ("Invalid header: " ++ s)
      return $ length cols - 1

    readField s = do
      let n = length s + 1
      unless (n `mod` 3 == 0) $
        Left ("Field is not aligned: " ++ s)
      return $ Field (filter (/= ' ') s) (n `div` 3) Nothing

    readRow n s = do
      let fields = filter (not.null) $ splitOn "|" s
      row <- traverse readField fields
      unless (sum (fieldSize <$> row) == n) $
        Left $ "Fields are not aligned at row\n " ++ s
      return row

=======
  
dataSample = concat
  ["011110000100011101111011101111110101010010010110",
   "111000010010111000011011111100010110100110100100"]





main = do 
  putStrLn diagram
  putStrLn "Parsed data"
  let d = read diagram :: Data Int
  print d
  putStrLn "\nSample data"
  putStrLn dataSample
  putStrLn "\nRestored fields"
  print $ readData d dataSample
>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15

