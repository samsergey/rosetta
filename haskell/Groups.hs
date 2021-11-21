import Data.Map ((!))
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Foldable

newtype Perm a = Perm { toMap :: (M.Map a a) }
  deriving Ord

instance Eq a => Eq (Perm a) where
  Perm a == Perm b = M.elems a == M.elems b

isId (Perm p) = M.elems p == M.keys p

instance (Eq a, Show a) => Show (Perm a) where
  show (Perm p)
    | isId (Perm p) = "e"
    | otherwise = "(" ++ unwords (show <$> M.elems p) ++ ")"

instance Ord a => Semigroup (Perm a) where
  Perm a <> Perm b = Perm $ M.map subst a <> (b M.\\ a)
    where
      subst x = fromMaybe x $ M.lookup x b

instance Ord a => Monoid (Perm a) where
  mempty = perm []

class Monoid m => Group m where
  inv :: m -> m

instance Ord a => Group (Perm a) where
  inv = Perm . M.fromList . map (\(a,b) -> (b,a)) . M.toList . toMap

perm p = Perm $ M.fromAscList $ (zip (sort p) p)

view k m = (\a -> (a, M.delete k m)) <$> M.lookup k m            

toCicles :: Ord a => Perm a -> [Perm a]
toCicles (Perm p) = Perm <$> unfoldr getCycle p
  where
    getCycle m
      | M.null m = Nothing
      | otherwise =
        let Just ((x,y), m') = M.minViewWithKey m
            go y r m
              | y == x = Just (r, m)
              | otherwise = let Just (z, m') = view y m
                            in go z (M.singleton y z <> r) m' 
        in go y (M.singleton x y) m'

act :: Ord b => Perm b -> [b] -> [b]
act (Perm p) = map (\x -> fromMaybe x (M.lookup x p))

permute :: Perm Int -> [a] -> [a]
permute (Perm p) lst = foldMap get p
  where
    get i = case splitAt (i-1) lst of
      (l,[]) -> mempty
      (a,x:b) -> [x]

canonicForm :: Ord a => Perm a -> Perm Int
canonicForm (Perm p) =
  let dict = M.fromList $ zip (M.elems p) [1..]
      rename x = fromJust $ M.lookup x dict
  in Perm $ M.mapKeys rename $ M.map rename p

orbit p = (p <> inv p) : takeWhile (not . isId) (iterate (p <>) p)

order p = length (orbit p)

------------------------------------------------------------

newtype PGroup = PGroup {elements :: [Perm Int]}

instance Show PGroup where
  show (PGroup ps) = "<group "++ show (length ps) ++ ">"

gOrder = length . elements

------------------------------------------------------------

rotations lst = take n $ take n <$> tails (cycle lst)
  where n = length lst

gD3 = PGroup $ perm <$> permutations [1,2,3]
gS n = PGroup $ perm <$> permutations [1..n]
gC n = PGroup $ perm <$> rotations [1..n]

edges p = [[a,b] | a <- p, b <- p, a < b]

actOnEdges es p = canonicForm . perm $ (perm p `act`) <$> es

gE n = PGroup $ actOnEdges (edges [1..n]) <$> permutations [1..n]
