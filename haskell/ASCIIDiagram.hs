import Data.List (nub)
import Data.List.Split (splitOn)
import Control.Monad (unless)

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


