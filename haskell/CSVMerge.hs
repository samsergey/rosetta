import Data.List
import Data.Maybe (mapMaybe, maybeToList)
import System.IO (readFile)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))

------------------------------------------------------------

type Assoc = [(String, String)]

newtype DB = DB { entries :: [Patient] }
  deriving Show

readDB :: String  -> DB
readDB = normalize
         . mapMaybe readPatient
         . readCSV

instance Semigroup DB where
  DB a <> DB b = normalize $ a <> b

instance Monoid DB where
  mempty = DB []

normalize :: [Patient] -> DB
normalize = DB
            . map mconcat 
            . groupBy (\x y -> pid x == pid y)
            . sortOn pid
  
------------------------------------------------------------

data Patient = Patient { pid :: String
                       , name :: Maybe String
                       , visits :: [String]
                       , scores :: [Float] }
  deriving Show

instance Semigroup Patient where
  p1 <> p2 = Patient
    (if null (pid p1) then pid p2 else pid p1)
    (name p1 <|> name p2)
    (visits p1 <|> visits p2)
    (scores p2 <|> scores p1)

instance Monoid Patient where
  mempty = Patient mempty mempty mempty mempty

readPatient r = do
  i <- lookup "PATIENT_ID" r
  let n = lookup "LASTNAME" r
  let d = lookup "VISIT_DATE" r >>= readDate
  let s = lookup "SCORE" r >>= readMaybe
  return $ Patient i n (maybeToList d) (maybeToList s)
  where
    readDate [] = Nothing
    readDate d = Just d
    
------------------------------------------------------------

readCSV :: String -> [Assoc]
readCSV txt = zip header <$> body
  where header:body = splitBy ',' <$> lines txt

splitBy ch = unfoldr go
  where
    go [] = Nothing
    go s  = Just $ drop 1 <$> span (/= ch) s

tabulateDB (DB ps) header cols = intercalate "|" <$> body
  where
    body = transpose $ zipWith pad width table
    table = transpose $ header : map showPatient ps
    showPatient p = sequence cols p
    width = maximum . map length <$> table
    pad n col = (' ' :) . take (n+1) . (++ repeat ' ') <$> col
    

main = do
  a <- readDB <$> readFile "patients.csv"
  b <- readDB <$> readFile "visits.csv"
  mapM_ putStrLn $ tabulateDB (a <> b) header fields
  where
    header = [ "PATIENT_ID", "LASTNAME", "VISIT_DATE"
             , "SCORES SUM","SCORES AVG"]
    fields = [ pid
             , \p -> case name p of {Nothing -> []; Just n -> n}
             , \p -> case visits p of {[] -> []; l -> last l}
             , \p -> case scores p of {[] -> []; s -> show (sum s)}
             , \p -> case scores p of {[] -> []; s -> show (mean s)} ]

    mean lst = sum lst / genericLength lst

