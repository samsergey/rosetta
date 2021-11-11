module Align where

-- The Wagner–Fischer matrix, adopted from (Levenstein distance)[https://rosettacode.org/wiki/Category:Haskell]. Matrix is reversed in order to simplify it's traversing. 
costs :: Eq a => [a] -> [a] -> [[Int]]
costs s1 s2 = reverse $ reverse <$> matrix
  where
    matrix = scanl transform [0 .. length s1] s2
    transform ns@(n:ns1) c = scanl calc (n + 1) $ zip3 s1 ns ns1
      where
        calc z (c1, x, y) = minimum [ y + 1, z + 1
                                    , x + fromEnum (c1 /= c)]

levenshteinDistance :: Eq a => [a] -> [a] -> Int
levenshteinDistance s1 s2 = head.head $ costs s1 s2

-- {{trans|Java}}
-- Instead of indices the Wagner–Fischer matrix and strings
-- are traversed by list truncation, as zippers. 
alignment :: String -> String -> (String, String)
alignment s1 s2 = go (costs s1 s2) (reverse s1) (reverse s2) ([],[])
  where
    go c _ _ r | isEmpty c = r
    go _ [] s r = ('-' <$ s, reverse s) <> r
    go _ s [] r = (reverse s, '-' <$ s) <> r
    go c s1@(h1:t1) s2@(h2:t2) (r1, r2) =
      let temp = (get.nextCol.nextRow $ c) + if h1 == h2 then 0 else 1
      in case get c of
        x | x == temp -> go (nextRow.nextCol $ c) t1 t2 (h1:r1, h2:r2)
          | x == 1 + (get.nextCol $ c) -> go (nextCol c) s1 t2 ('-':r1, h2:r2)
          | x == 1 + (get.nextRow $ c) -> go (nextRow c) t1 s2 (h1:r1, '-':r2)

    -- Functions which treat table as zipper
    get ((h:_):_) = h
    nextRow = map tail
    nextCol = tail
    isEmpty c = null c || null (head c)

{- 
<pre>λ> alignment "palace" "place"
("palace","p-lace")

λ> alignment "rosettacode" "raisethysword"
("r-oset-tacode","raisethysword")

λ> alignment "rosettacode" "rat"
("rosettacode","r-----a---t")</pre>
-}

-- Alternative solution, producing all minimal alignments for two strings.


-- Produces all possible alignments for two strings.
allAlignments :: String -> String -> [[(Char, Char)]]
allAlignments s1 s2 = go (length s2 - length s1) s1 s2
  where
    go _ s [] = [(\x -> (x, '-')) <$> s]
    go _ [] s = [(\x -> ('-' ,x)) <$> s]
    go n s1@(h1:t1) s2@(h2:t2) = (h1, h2) <:> go n t1 t2
      ++ case compare n 0 of
           LT -> (h1, '-') <:> go (n+1) t1 s2
           EQ -> []
           GT -> ('-', h2) <:> go (n-1) s1 t2

    x <:> l = fmap (x :) l

-- Returns a lazy list of all optimal allAlignments.
levenshteinAlignments :: String -> String -> [(String, String)]
levenshteinAlignments s1 s2 = unzip <$> best
  where
    best = filter ((lev ==) . dist) $ allAlignments s1 s2
    lev = levenshteinDistance s1 s2
    dist = length . filter (uncurry (/=))

{-
<pre>λ> mapM_ print $ levenshteinAlignments "rosettacode" "raisethysword"
("ro-settac-ode","raisethysword")
("ro-setta-code","raisethysword")
("ro-sett-acode","raisethysword")
("ro-set-tacode","raisethysword")
("r-osettac-ode","raisethysword") 
("r-osetta-code","raisethysword")
("r-osett-acode","raisethysword")
("r-oset-tacode","raisethysword")

λ> mapM_ print $ levenshteinAlignments "rosettacode" "rat"
("rosettacode","ra--t------")
("rosettacode","ra---t-----")
("rosettacode","r-a-t------")
("rosettacode","r-a--t-----")
("rosettacode","r--at------")
("rosettacode","r--a-t-----")
("rosettacode","r---at-----")
("rosettacode","r-----at---")
("rosettacode","r-----a-t--")
("rosettacode","r-----a--t-")
("rosettacode","r-----a---t")</pre>
-}
