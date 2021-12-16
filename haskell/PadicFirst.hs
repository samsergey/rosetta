{-# LANGUAGE KindSignatures, DataKinds  #-}
module Padic where

import Data.Ratio
import Data.List (genericLength, find)
import GHC.TypeLits 

data Padic (n :: Nat) = Null
                      | Padic { unit :: [Int]
                              , val :: Int }

-- valuation od the base
modulo :: (KnownNat p, Integral i) => Padic p -> i
modulo = fromIntegral . natVal

-- Constructor for zero value
pZero :: KnownNat p => Padic p
pZero = Padic (repeat 0) maxBound

-- Smart constructor, adjusts trailing zeros with the valuation.
mkPadic :: (KnownNat p, Integral i)
        => [i] -> Int -> Padic p
mkPadic u v = go 0 (fromIntegral <$> u)
  where
    go 17 _ = pZero
    go i (0:u) = go (i+1) u
    go i u = Padic u (v+i)

-- Constructor for p-adic unit
mkUnit :: (KnownNat p, Integral i) => [i] -> Padic p
mkUnit u = mkPadic u 0

-- Zero test (up to 1/p^17)
isZero :: KnownNat p => Padic p -> Bool
isZero (Padic u _) = all (== 0) (take 17 u)
isZero _ = False

-- p-adic norm
pNorm :: KnownNat p => Padic p -> Ratio Int
pNorm Null = undefined
pNorm p = fromIntegral (modulo p) ^^ (- val p)

-- test for an integerness up to p^-17
isInteger :: KnownNat p => Padic p -> Bool
isInteger Null = False
isInteger (Padic s v) = case splitAt (-v) s of
  ([],i) -> length (takeWhile (==0) $ reverse (take 20 i)) > 3
  _ -> False

-- p-adics are shown with 1/p^17 precision
instance KnownNat p => Show (Padic p) where
  show Null = "Null"
  show x@(Padic u v) =
    show (modulo x) ++ "-adic: " ++
    (case si of {[] -> "0"; _ -> si})
    ++ "." ++
    (case f of {[] -> "0"; _ -> sf})
    where
      (f,i) = case compare v 0 of
        LT -> splitAt (-v) (u ++ repeat 0)
        EQ -> ([], u)
        GT -> ([], replicate v 0 ++ u)
      sf = foldMap showD $ reverse $ take 17 f
      si = foldMap showD $ dropWhile (== 0) $ reverse $ take 17 i
      el s = if length s > 16 then "…" else ""
      showD n = [(['0'..'9']++['a'..'z']) !! n]

instance KnownNat p => Eq (Padic p) where
  Null == Null = True
  a == b = isZero (a - b)

instance KnownNat p => Ord (Padic p) where
  compare = error "Ordering is undefined fo p-adics."

instance KnownNat p => Num (Padic p) where
  fromInteger 0 = pZero
  fromInteger n = pAdic (fromInteger n)

  x@(Padic a va) + Padic b vb = mkPadic s v
    where
      v = va `min` vb
      s = addMod (modulo x)
          (replicate (va - v) 0 ++ a)
          (replicate (vb - v) 0 ++ b)
  _ + _ = Null

  x@(Padic a va) * Padic b vb =
    mkPadic (mulMod (modulo x) a b) (va + vb)
  _ * _ = Null
  
  negate x@(Padic u v) =
    case map (\y -> modulo x - 1 - y) u of
      n:ns -> Padic ((n+1):ns) v
      [] -> pZero      
  negate _ = Null
  
  abs p = pAdic (pNorm p)

  signum = undefined

------------------------------------------------------------
-- conversion from rationals to p-adics

instance KnownNat p => Fractional (Padic p) where
  fromRational = pAdic
     
  recip Null = Null
  recip x@(Padic (u:us) v)
    | isZero x = Null
    | gcd p u /= 1 = Null
    | otherwise = mkPadic res (-v)
    where
      p = modulo x
      res = longDivMod p (1:repeat 0) (u:us)

pAdic :: (Show i, Integral i, KnownNat p)
      => Ratio i -> Padic p
pAdic 0 = pZero
pAdic x = res 
  where
    p = modulo res
    (v, q) = getUnit p x
    (n, d) = (numerator q, denominator q)
    res = maybe Null process $ recipMod p d

    process r = mkPadic (series n) v
      where
        series n
          | n == 0 = repeat 0
          | n `mod` p == 0 = 0 : series (n `div` p)
          | otherwise =
              let m = (n * r) `mod` p
              in m : series ((n - m * d) `div` p)

------------------------------------------------------------
-- conversion from p-adics to rationals
-- works for relatively small denominators

instance KnownNat p => Real (Padic p) where
  toRational Null = error "no rational representation!"
  toRational x@(Padic s v) = res
    where
      p = modulo x
      res = case break isInteger $ take 10000 $ iterate (x +) x of
        (_,[]) -> - toRational (- x)
        (d, i:_) -> (fromBase p (unit i) * (p^(- val i))) % (genericLength d + 1)
        
      fromBase p = foldr (\x r -> r*p + x) 0 .
                   take 20 . map fromIntegral
                   

------------------------------------------------------------



--------------------------------------------------------------------------------
-- helper functions

-- extracts p-adic unit from a rational number
getUnit :: Integral i => i -> Ratio i -> (Int, Ratio i)
getUnit p x = (genericLength v2 - genericLength v1, c) 
  where
    (v1,b:_) = span (\n -> denominator n `mod` p == 0) $
               iterate (* fromIntegral p) x
    (v2,c:_) = span (\n -> numerator n `mod` p == 0) $
               iterate (/ fromIntegral p) b

    
-- Reciprocal of a number modulo p (extended Euclidean algorythm).
-- For non-prime p returns Nothing non-inverible element of the ring.
recipMod :: Integral i => i -> i -> Maybe i
recipMod p 1 = Just 1
recipMod p a | gcd p a == 1 = Just $ go 0 1 p a
             | otherwise = Nothing
  where
    go t _ _ 0 = t `mod` p
    go t nt r nr =
      let q = r `div` nr
      in go nt (t - q*nt) nr (r - q*nr)

-- Addition of two sequences modulo p
addMod p = go 0
  where
    go 0 [] ys = ys
    go 0 xs [] = xs
    go s [] ys = go 0 [s] ys
    go s xs [] = go 0 xs [s]
    go s (x:xs) (y:ys) =
      let (q, r) = (x + y + s) `divMod` p
      in r : go q xs ys

subMod p a (b:bs) = addMod p a $ (p-b) : ((p - 1 -) <$> bs)

-- Multiplication of two sequences modulo p
mulMod p as [b] = mulMod p [b] as
mulMod p as bs = case as of
  [0] -> repeat 0
  [1] -> bs
  [a] -> go 0 bs
    where
      go s [] = [s]
      go s (b:bs) =
        let (q, r) = (a * b + s) `divMod` p
        in r : go q bs
  as -> go bs
    where
      go [] = []
      go (b:bs) =
        let c:cs = mulMod p [b] as
        in c : addMod p (go bs) cs

longDivMod p a (b:bs) = case recipMod p b of
  Nothing -> error $
    show b ++ " is not invertible modulo " ++ show p
  Just r -> go a
    where
      go [] = []
      go (0:xs) = 0 : go xs
      go (x:xs) =
        let m = (x*r) `mod` p
            _:zs = subMod p (x:xs) (mulMod p [m] (b:bs))
        in m : go zs

----------------------------------------------------------------------

newton f a = limit $ iterate f a

limit (x:y:z) | x == y = x
              | otherwise = limit (y:z)

recip' Null = Null
recip' p@(Padic (u:us) v) =
  case recipMod (modulo p) u of
    Just a -> newton (\x -> 2*x - p*x*x) (mkUnit [a])
    Nothing -> Null

pSqrt :: KnownNat p => Padic p -> Padic p
pSqrt Null = Null
pSqrt c@(Padic (u:us) v) =
  case sqrtMod (modulo c) u of
    Just a -> newton (\x -> x/2 + c/(2*x)) (mkUnit [a])
    Nothing -> Null

eqMod p a b = a `mod` p == b `mod` p
sqrtMod p a = find (\x -> eqMod p (x*x) a) [1..p-1]

fromBase b lst = foldr (\x r -> r*b + x) 0 lst

-- oRat :: ((Integer,Integer),(Integer,Integer))
-- toRat x = go  (s,1) (p^9, 0)
--   where
--     p = modulo x
--     s = fromBase p (take 10 (unit x))
--     go (u1,u2) (v1,v2)
--       | v1*v1 + v2*v2 < u1*u1 + u2*u2 = let
--           q = (u1*v1 + u2*v2) `div` (v1*v1 + v2*v2)
--           in go (v1,v2) (u1 - q*v1, u2 - q*v2)
--       | otherwise = ((u1,u2), (v1,v2))
      
      
