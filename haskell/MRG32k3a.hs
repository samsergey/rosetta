import Data.Bits
import Data.Word
import System.Random
import Data.List

newtype MRG32k3a = MRG32k3a ([Int], [Int])
  deriving Show

mkMRG32k3a s = MRG32k3a ([s,0,0],[s,0,0])

instance RandomGen MRG32k3a where
  next (MRG32k3a (x1,x2)) =
    let x1i = sum (zipWith (*) x1 a1) `mod` m1
        x2i = sum (zipWith (*) x2 a2) `mod` m2
    in ((x1i - x2i) `mod` m1, MRG32k3a (x1i:init x1, x2i:init x2))
    where
      a1 = [0, 1403580, -810728]
      m1 = 2^32 - 209
      a2 = [527612, 0, -1370589]
      m2 = 2^32 - 22853

  split _ = error "MRG32k3a is not splittable"

randomsFloat :: MRG32k3a -> [Float] 
randomsFloat s = map ((/ (2^32 - 208)) . fromIntegral) $ (randoms s :: [Int])

------------------------------------------------------------

data PCGen = PCGen !Word64 !Word64 

mkPCGen state sequence =
  let
    n = 6364136223846793005 :: Word64 
    inc = (sequence `shiftL` 1) .|. 1 :: Word64 
  in PCGen ((inc + state)*n + inc) inc 

instance RandomGen PCGen where
   next (PCGen state inc) =
     let
       n = 6364136223846793005 :: Word64
       xs = fromIntegral $ ((state `shiftR` 18) `xor` state) `shiftR` 27 :: Word32
       rot = fromIntegral $ state `shiftR` 59 :: Int
     in (fromIntegral $ (xs `shiftR` rot) .|. (xs `shiftL` ((-rot) .&. 31))
        , PCGen (state * n + inc) inc)

   split _ = error "PCG32 is not splittable"

randoms' :: RandomGen g => g -> [Int]
randoms' g = unfoldr (pure . next) g
