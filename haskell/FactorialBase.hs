import Data.List (unfoldr, intercalate)

newtype Fact = Fact [Int]

-- smart constructor
fact :: [Int] -> Fact
fact = Fact . zipWith min [0..] . reverse

instance Show Fact where
  show (Fact ds) = intercalate "." $ show <$> reverse ds
    
toFact :: Integer -> Fact
toFact 0 = Fact [0]
toFact n = Fact $ unfoldr f (1, n)
  where
    f (b, 0) = Nothing
    f (b, n) = let (q, r) = n `divMod` b
               in Just (fromIntegral r, (b+1, q))

fromFact :: Fact -> Integer
fromFact (Fact ds) = foldr f 0 $ zip [1..] ds
  where
    f (b, d) r = r * b + fromIntegral d

------------------------------------------------------------

toPermutation :: Fact -> [Int]
toPermutation (Fact ds) = go (reverse ds) [0.. length ds - 1]
  where
    go [] p = p
    go (d:ds) p = case splitAt d p of
                    (a,x:b) -> x : go ds (a++b)
                    (a,[]) -> a

permute :: [a] -> [Int] -> [a]
permute s p = case splitAt (length s - length p) s of
                (s1,s2) -> s1 ++ map (s2 !!) p

------------------------------------------------------------

task1 = do
  putStrLn "number\tfactoradic\tpermutation"
  mapM_ display [0..23]
  where
    display n =
      let f = toFact n
          p = permute "0123" (toPermutation f)
      in putStrLn $ show n ++ "\t" ++ show f ++ "\t\t(" ++ p ++ ")"

randomFactDigits seed = zipWith mod (random seed) [1..]
  where
    random = iterate $ \x -> (x * 1103515245 + 12345) `mod` (2^31-1)

task2 = do
  putStrLn "-- First example --"
  let n1 = toFact 61988771037597375208735783409763169805823569176280269403732950003152
  let crate1 = permute crate $ toPermutation n1
  putStrLn $ "Factoradic number:\n" ++ show n1
  putStrLn $ "Corresponding crate permutation:\n" ++ unwords crate1
  
  putStrLn "\n-- Second example --"
  let n2 = toFact 80576939285541005152259046665383499297948014296200417968998877609223
  let crate2 = permute crate $ toPermutation n2
  putStrLn $ "Factoradic number:\n" ++ show n2
  putStrLn $ "Corresponding crate permutation:\n" ++ unwords crate2
  
  putStrLn "\n-- Random example --"
  let n3 = Fact $ take 52 $ randomFactDigits 42
  let crate3 = permute crate $ toPermutation n3
  putStrLn $ "Factoradic number:\n" ++ show n3
  putStrLn $ "Decimal representation of n:\n" ++ show (fromFact n3)
  putStrLn $ "Corresponding crate permutation:\n" ++ unwords crate3
  where
  crate = words "A♠ K♠ Q♠ J♠ 10♠ 9♠ 8♠ 7♠ 6♠ 5♠ 4♠ 3♠ 2♠\
\                A♥ K♥ Q♥ J♥ 10♥ 9♥ 8♥ 7♥ 6♥ 5♥ 4♥ 3♥ 2♥\
\                A♦ K♦ Q♦ J♦ 10♦ 9♦ 8♦ 7♦ 6♦ 5♦ 4♦ 3♦ 2♦\
\                A♣ K♣ Q♣ J♣ 10♣ 9♣ 8♣ 7♣ 6♣ 5♣ 4♣ 3♣ 2♣"

