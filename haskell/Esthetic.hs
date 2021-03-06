<<<<<<< HEAD
{-# LANGUAGE FlexibleContexts #-}
import Data.List (unfoldr)

isEsthetic b = all ((== 1) . abs) . difs . digits b

difs lst = zipWith (-) lst (tail lst)

digits b = unfoldr f
=======
import Data.List (unfoldr, genericIndex, uncons)
import Control.Monad (replicateM, foldM, mzero)

-- representation of numbers as digits
fromBase b = foldM f 0
  where f r d | d < 0 || d >= b = mzero
              | otherwise = return (r*b + d)

toBase b = reverse . unfoldr f
>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15
  where
    f 0 = Nothing
    f n = let (q, r) = divMod n b in Just (r, q)

<<<<<<< HEAD
main = print $ filter (isEsthetic 10) [100000000..130000000]
=======
showInBase b = foldMap (pure . digit) . toBase b
  where digit = genericIndex (['0'..'9'] <> ['a'..'z'])

-- a predicate for esthetic numbers
isEsthetic b = all ((== 1) . abs) . differences . toBase b
  where
    differences lst = zipWith (-) lst (tail lst)

-- infinite list of esthetic numbers
esthetics_m b =
  do differences <- (\n -> replicateM n [-1, 1]) <$> [0..]
     firstDigit <- [1..b-1]
     differences >>= fromBase b <$> scanl (+) firstDigit

task2 b = do
  putStrLn $ "Task for base " <> show b
  putStrLn $ unwords $ showInBase b <$> (drop (b*4-1) . take (b*6) $ esthetics b)

takeWithin a b = dropWhile (< a) . takeWhile (<= b)

task3 a b = dropWhile (< a) . takeWhile (<= b) $ esthetics 10

esthetics b = tail $ fst <$> iterate step (undefined, q)
  where
    q = [(d, d) | d <- [1..b-1]]
    step (_, queue) =
      let (num, lsd) = head queue
          new_lsds = [d | d <- [lsd-1, lsd+1], d < b, d >= 0]
      in (num, tail queue ++ [(num*b + d, d) | d <- new_lsds])
      

main = print ()
>>>>>>> 231348938c4036a6d68d0d625c9b3c476363cc15
