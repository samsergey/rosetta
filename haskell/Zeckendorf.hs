{-# LANGUAGE LambdaCase #-}
import Data.List (find, mapAccumL)
import Control.Arrow (first, second)
import Test.QuickCheck

fibs :: Num a => a -> a -> [a]
fibs a b = res
  where
    res = a : b : zipWith (+) res (tail res)

data Fib = Fib { sign :: Int
               , digits :: [Int]}

-- smart constructor
mkFib s ds =
  case dropWhile (==0) ds of
    [] -> 0
    ds -> Fib s (reverse ds)

-- textual representation
instance Show Fib where
  show (Fib s ds) = sig s ++ foldMap show (reverse ds)
    where sig = \case { -1 -> "-"; s -> "" }

-- equivalence for Zeckendorf numbers
instance Eq Fib where
  Fib sa a == Fib sb b = sa == sb && a == b

-- ordering for Zeckendorf numbers
instance Ord Fib where
  a `compare` b =
    sign a `compare` sign b <>
    case find (/= 0) $ alignWith (-) (digits a) (digits b) of
      Nothing -> EQ
      Just 1 -> if sign a > 0 then GT else LT
      Just (-1) -> if sign a > 0 then LT else GT

-- arithmetic for Zeckendorf numbers  
instance Num Fib where
  negate (Fib s ds) = Fib (negate s) ds
  abs (Fib s ds) = Fib 1 ds
  signum (Fib s _) = fromIntegral s

  fromInteger n =
    case compare n 0 of
      LT -> negate $ fromInteger (- n)
      EQ -> Fib 0 [0]
      GT -> Fib 1 . reverse . fst $ divModFib n 1

  0 + a = a
  a + 0 = a
  a + b =
    case (sign a, sign b) of
      ( 1, 1) -> res
      (-1, 1) -> b - (-a)
      ( 1,-1) -> a - (-b)
      (-1,-1) -> - ((- a) + (- b))
    where
      res = mkFib 1 . process $ 0:0:c
      c = alignWith (+) (digits a) (digits b)
       -- use cellular automata
      process =
        runRight' 3 r2 . runLeft 3 r2 . runRight 4 r1

  0 - a = -a
  a - 0 = a
  a - b =
    case (sign a, sign b) of
      ( 1, 1) -> res
      (-1, 1) -> - ((-a) + b)
      ( 1,-1) -> a + (-b)
      (-1,-1) -> - ((-a) - (-b))  
    where
      res = case find (/= 0) c of
        Just 1  -> mkFib 1 . process $ c
        Just (-1) -> - (b - a)
        Nothing -> 0
      c = alignWith (-) (digits a) (digits b)
      -- use cellular automata
      process =
        runRight' 3 r2 . runLeft 3 r2 . runRight 4 r1 . runRight' 3 r3

  0 * a = 0
  a * 0 = 0
  1 * a = a
  a * 1 = a
  a * b =
    case (sign a, sign b) of
      (1, 1) -> res
      (-1, 1) -> - ((-a) * b)
      ( 1,-1) -> - (a * (-b))
      (-1,-1) -> ((-a) * (-b))  
    where
      -- use zeckendorf table
      table = fibs a (a + a)
      res = sum $ onlyOnes $ zip (digits b) table
      onlyOnes = map snd . filter ((==1) . fst)

instance Enum Fib where
  toEnum = fromInteger . fromIntegral
  fromEnum = fromIntegral . toInteger
  
instance Real Fib where
  toRational = fromInteger . toInteger
  
instance Integral Fib where
  -- reconstruction to integers
  toInteger (Fib s ds) = signum (fromIntegral s) * res
    where
      res = sum (zipWith (*) (fibs 1 2) (fromIntegral <$> ds))

  quotRem 0 _ = (0, 0)
  quotRem a 0 = error "divide by zero"
  quotRem a b = case (sign a, sign b) of
      (1, 1) -> first (mkFib 1) $ divModFib a b
      (-1, 1) -> second negate . first negate $ quotRem (-a) b
      ( 1,-1) -> first negate $ quotRem a (-b)
      (-1,-1) -> second negate $ quotRem (-a) (-b) 

------------------------------------------------------------
-- helper funtions

-- general division using zeckendorf table
divModFib :: (Ord a, Num c, Num a) => a -> a -> ([c], a)
divModFib a b = (q, r)
  where
    (r, q) = mapAccumL f a $ reverse $ takeWhile (<= a) table
    table = fibs b (b+b)
    f n x = if  n < x then (n, 0) else (n - x, 1)

-- application of rewriting rules
-- runs window from left ro right
runRight' n f = go
  where
    go []  = []
    go lst = let (w, r) = splitAt n lst 
                 (h: t) = f w
             in h : go (t ++ r)
                    
-- runs window from left ro right and reverses the result
runRight n f = go []
  where
    go res []  = res
    go res lst = let (w, r) = splitAt n lst 
                     (h: t) = f w
                 in go (h : res) (t ++ r)

-- runs reversed window function and reverses the result
runLeft n f = runRight n (reverse . f . reverse) 

-- rewriting rules from [C. Ahlbach et. all]
r1 = \case [0,3,0]   -> [1,1,1]
           [0,2,0]   -> [1,0,1]
           [0,1,2]   -> [1,0,1]
           [0,2,1]   -> [1,1,0]
           [x,0,2]   -> [x,1,0]
           [x,0,3]   -> [x,1,1]
           [0,1,2,0] -> [1,0,1,0]
           [0,2,0,x] -> [1,0,0,x+1]
           [0,3,0,x] -> [1,1,0,x+1]
           [0,2,1,x] -> [1,1,0,x  ]
           [0,1,2,x] -> [1,0,1,x  ]
           l -> l

r2 = \case [0,1,1] -> [1,0,0]
           l -> l

r3 = \case [1,-1]    -> [0,1]
           [2,-1]    -> [1,1]
           [1, 0, 0] -> [0,1,1]
           [1,-1, 0] -> [0,0,1]
           [1,-1, 1] -> [0,0,2]
           [1, 0,-1] -> [0,1,0]
           [2, 0, 0] -> [1,1,1]
           [2,-1, 0] -> [1,0,1]
           [2,-1, 1] -> [1,0,2]
           [2, 0,-1] -> [1,1,0]
           l -> l

alignWith :: (Int -> Int -> a) -> [Int] -> [Int] -> [a]
alignWith f a b = go [] a b
  where
    go res as [] = ((`f` 0) <$> reverse as) ++ res
    go res [] bs = ((0 `f`) <$> reverse bs) ++ res
    go res (a:as) (b:bs) = go (f a b : res) as bs

------------------------------------------------------------

instance Arbitrary Fib where
  arbitrary = fromInteger <$> arbitrary

pIsomorphism1 :: Fib -> Bool
pIsomorphism1 x = fromInteger (toInteger x) == x

pIsomorphism2 :: Integer -> Bool
pIsomorphism2 x = toInteger (fromInteger x) == x

pAddCommutative :: Fib -> Fib -> Bool
pAddCommutative a b = a + b == b + a

pAddAssoc :: Fib -> Fib -> Fib -> Bool
pAddAssoc a b c = a + (b + c) == (a + b) + c

pMulCommutative :: Fib -> Fib -> Bool
pMulCommutative a b = a * b == b * a

pMulAssoc :: Fib -> Fib -> Fib -> Bool
pMulAssoc a b c = a * (b * c) == (a * b) * c

pMulDistr :: Fib -> Fib -> Fib -> Bool
pMulDistr a b c = a * (b + c) == a * b + a * c

pAddHomomorphism :: Integer -> Integer -> Bool
pAddHomomorphism a b = 
  fromIntegral a + fromIntegral b == (fromIntegral (a + b) :: Fib)

pSubHomomorphism :: Integer -> Integer -> Bool
pSubHomomorphism a b =
  fromIntegral a - fromIntegral b == (fromIntegral (a - b) :: Fib)

pMulHomomorphism :: Integer -> Integer -> Bool
pMulHomomorphism a b =
  fromIntegral a * fromIntegral b == (fromIntegral (a * b) :: Fib)

pDivMod :: Fib -> Fib -> Bool
pDivMod a b =
  if b /= 0
  then let (q, r) = divMod a b in a == b*q + r
  else True
  

pOrdering :: Integer -> Integer -> Bool
pOrdering a b =
  a `compare` b == fromIntegral a `compare` (fromIntegral b :: Fib)


tests = do
  quickCheck pIsomorphism1
  quickCheck pIsomorphism2
  quickCheck pAddCommutative
  quickCheck pAddAssoc
  quickCheck pMulCommutative
  quickCheck pMulAssoc
  quickCheck pMulDistr
  quickCheck pAddHomomorphism
  quickCheck pSubHomomorphism
  quickCheck pMulHomomorphism
  quickCheck pOrdering
  quickCheck pDivMod
