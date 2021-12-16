{-# LANGUAGE UnicodeSyntax, Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.InfList
-- Copyright   :  (c) James H. Fisher, 2012
-- License     :  Modified BSD License
-- 
-- Maintainer  :  jameshfisher@gmail.com
--
-- This module provides an infinite list type and operations thereon.
--
-- Haskell has two outstanding features:
--
-- * laziness, which in particular allows infinite data structures;
--
-- * a very strong type system.
--
-- In that light, it's a bit embarrassing that its default libraries do not allow
-- one to express the infinitude of, say, a list.  For example, 'Prelude.repeat'
-- has type @a → [a]@, when it of course should not return a finite list.
--
-- This module defines an infinite list data structure and provides all the
-- standard list-manipulating functions that make sense on them.
-- The advantages of using this over a standard list are:
--
-- * /Type safety and clarity./
--   We are prevented from using nonsensical functions on infinite lists.
--   These include the functions: 'Data.List.last', 'Data.List.init',
--   'Data.List.null', 'Data.List.length', 'Data.List.foldl', 'Data.List.scanr',
--   'Data.List.scanl', 'Data.List.replicate', 'Data.List.reverse',
--   'Data.List.any' (etc.), 'Data.List.isSuffixOf', 'Data.List.(\\)',
--   'Data.List.union', 'Data.List.unionBy', 'Data.List.intersect',
--   'Data.List.intersectBy', 'Data.List.transpose', 'Data.List.deleteFirstsBy',
--   'Data.List.subsequences', 'Data.List.nonEmptySubsequences',
--   'Data.List.permutations', 'Data.List.sort', 'Data.List.sortBy',
--   'Data.List.sum' (etc.), 'Data.List.elem' (etc.), 'Data.List.lookup',
--   'Data.List.findIndex', 'Data.List.elemIndex', 'Data.List.isInfixOf',
--   'Data.List.nub'.
--
-- * /No pattern-matching on nil/.
--   Where you can identify that @[]@ will never be present,
--   using a list forces you to either write clunky error-generating pattern matches,
--   or alternatively put up with compiler warnings about incompete pattern matches.
--   Code is simplified.
--
-- * /Faster code./
--   We do not need or want runtime pattern-matches on a data constructor which
--   will never occur.  Haskell compilers love single-constructor datatypes:
--   <http://www.haskell.org/haskellwiki/Performance/Data_types#Single-constructor_datatypes>
--
-- See for comparison the 'GHC.List' and 'Data.List' modules.
--
-----------------------------------------------------------------------------

module InfList
  ( -- * Data type
    InfList(..)

    -- * Type synonyms
  , Pred

    -- * Infinite-list producer replacements
  , iterate       -- (a → a) → a → InfList a
  , repeat        -- a → InfList a
  , cycle         -- [a] → InfList a

    -- * Operations on 'InfList'

    -- ** Accessors
  , head          -- InfList a → a
  , tail          -- InfList a → InfList a

    -- ** Conversion to and from lists
  , toList        -- InfList a → [a]
  , fromList      -- [a] → InfList a

    -- ** Maps
  , map           -- (a → b) → InfList a → InfList b
  , mapAccumL     -- (acc → a → (acc,b)) → acc → InfList a → InfList b

    -- ** Folds
  , foldr         -- (a → b → b) → InfList a → b
  , unfoldr       -- (a → (b,a)) → a → InfList b

    -- ** Concatenating, interspersing, glueing
  , intersperse   -- a → InfList a → InfList a
  , intercalate   -- [a] → InfList [a] → InfList a
  , (+++)         -- [a] → InfList a → InfList a
  , concatMap     -- (a → [b]) → InfList a → InfList b
  , concat        -- InfList [a] → InfList a

  -- ** Splitting
  , splitAt       -- Int → InfList a → ([a], InfList a)
  , span          -- Pred a → InfList a → ([a], InfList a)
  , break         -- Pred a → InfList a → ([a], InfList a)
  , groupBy       -- (a → a → Bool) → InfList a → InfList [a]
  , group         -- Eq a ⇒ InfList a → InfList [a]
  , partition     -- Pred a → InfList a → (InfList a, InfList a)

  -- ** Prefixes and suffixes
  , isPrefixOf    -- (Eq a) ⇒ [a] → InfList a → Bool
  , stripPrefix   -- Eq a ⇒ [a] → InfList a → Maybe (InfList a)
  , inits         -- InfList a → InfList [a]
  , tails         -- InfList a → InfList (InfList a)
  , take          -- Int → InfList a → [a]
  , drop          -- Int → InfList a → InfList a
  , takeWhile     -- Pred a → InfList a → [a]
  , dropWhile     -- Pred a → InfList a → InfList a

  -- ** Zip functions
  , zip           -- InfList a → InfList b → InfList (a,b)
  , zip3          -- InfList a → InfList b → InfList c → InfList (a,b,c)
  , zipWith       -- (a → b → c) → InfList a → InfList b → InfList c
  , zipWith3      -- (a → b → c → d) → InfList a → InfList b → InfList c → InfList d
  , unzip         -- InfList (a,b) → (InfList a, InfList b)
  , unzip3        -- InfList (a,b,c) → (InfList a, InfList b, InfList c)

  -- ** Dropping and deleting of elements
  , filter        -- Pred a → InfList a → InfList a
  , deleteBy      -- (Eq a) ⇒ Pred a → InfList a → InfList a
  , delete        -- (Eq a) ⇒ a → InfList a → InfList a

  -- ** Index functions
  , (!!!)         -- InfList a → Int → a
  , findIndices   -- Pred a → InfList a → InfList Int
  , elemIndices   -- Eq a ⇒ a → InfList a → InfList Int
  ) where


--------------------------------------------------------------------------------
-- Imports

import Prelude 
    hiding (map, filter, foldr, iterate, cycle, takeWhile, dropWhile, take,
    drop, splitAt, span, concatMap, concat, zip, zip3, zipWith, zipWith3,
    unzip, unzip3, lines, break, unlines, words, unwords, head, tail, repeat)


--------------------------------------------------------------------------------
-- Data types

-- how tight should this bind?
-- (:) is infixr 5
infixr 5 :::

-- | The infinite list data type.
-- This is identical to the normal list data type
-- with the sole exception that it lacks the @[]@ constructor.
data InfList a =
  -- | Cons an element onto an infinite list to yield another infinite list.
  a ::: InfList a 

-- | A simple type synonym for predicates.
type Pred a = a → Bool


--------------------------------------------------------------------------------
-- Typeclass instances

-- | `fmap` is the same for 'InfList' as for lists.
instance Functor InfList where
  fmap = map

-- | Be careful. Equality causes non-termination.
instance (Eq a) ⇒ Eq (InfList a) where
  (x:::xs) == (y:::ys) = (x == y) && (xs == ys)

-- | Be careful. Equality causes non-termination.
instance (Ord a) ⇒ Ord (InfList a) where
  compare (x:::xs) (y:::ys) = case compare x y of
                              EQ    → compare xs ys
                              other → other

-- | An 'InfList' looks the same as a list,
-- with "InfList " prepended.
instance (Show a) ⇒ Show (InfList a) where
  show l = "InfList [" ++ show' l where
    show' (x:::xs) = show x ++ "," ++ show' xs


--------------------------------------------------------------------------------
-- Operations

-- | Catamorphism.
-- This is not the same as 'Data.List.foldr' on lists,
-- as it contains no base value.
foldr ::
  (a → b → b) -- ^ A replacement for the ':::' constructor
  → InfList a -- ^ The 'InfList' to replace it in
  → b
foldr cons (x:::xs) = cons x (foldr cons xs)

-- | Convert an 'InfList' to an infinite list.
-- Use sparingly.
toList ::
  InfList a -- ^ the 'InfList'
  → [a]     -- ^ the normal infinite list
toList = foldr (:)

-- | Convert an infinite list to an 'InfList'.
-- Throws an error if the given list runs out of elements.
-- Use sparingly.
fromList ::
  [a]         -- ^ an infinite list
  → InfList a -- ^ the corresponding InfList
fromList []     = error "Data.InfList.fromList: null list"
fromList (x:xs) = x ::: fromList xs

-- | Get the first element of a list.
-- This is guaranteed to exist.
head ::
  InfList a -- ^ the list
  → a       -- ^ its first element
head (x:::_) = x

-- | Get the tail of a list.
-- This is guaranteed to exist.
tail ::
  InfList a   -- ^ the list
  → InfList a -- ^ its tail
tail (_:::xs) = xs

-- | Apply a function to each element of the list,
-- yielding a new list of the results.
map ::
  (a → b)     -- ^ an element mapping function
  → InfList a -- ^ a list of elements
  → InfList b -- ^ the result of applying the function to each list element
map f (x:::xs) = f x ::: map f xs

-- | Select the elements of a list that satisfy a predicate.
filter ::
  Pred a      -- ^ a predicate on list elements
  → InfList a -- ^ the list of elements
  → InfList a -- ^ all elements of the list that satisfy the predicate
filter p (x:::xs)
  | p x    = x ::: rest
  | otherwise = rest
  where rest = filter p xs

-- | Repeatedly apply a generating function on an initial value,
-- generating an infinite list.
iterate ::
  (a → a)     -- ^ the element-generating function
  → a         -- ^ the initial element
  → InfList a -- ^ the infinite list of elements
iterate f x = x ::: iterate f (f x)

-- | Given a list element, use that as every element in an infinite list.
repeat ::
  a           -- ^ The list element
  → InfList a -- ^ the infinite list where each element is that element
repeat x = xs where xs = x:::xs

-- | Prepend a finite list to a infinite list
-- yielding a new infinite list.
(+++) ::
  [a]          -- ^ a finite list
  → InfList a  -- ^ an infinite list
  → InfList a  -- ^ the finite list prepended to the infinite list
[]     +++ l = l
(x:xs) +++ l = x:::(xs +++ l)

-- | Repeat the elements of a non-empty finite list.
cycle ::
  [a]          -- ^ A non-empty finite list
  → InfList a  -- ^ The elements of that list repeated infinitely
cycle [] = error "Data.InfList: empty list"
cycle xs = xs +++ cycle xs

-- | The longest prefix of an infinite list
-- such that all elements of that prefix satisfy a given predicate.
takeWhile ::
  Pred a      -- ^ a predicate on list elements
  → InfList a -- ^ an infinite list
  → [a]       -- ^ the satisfying prefix of that list
takeWhile p (x:::xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

-- | Remove the longest prefix of an infinite list
-- where all elements of that prefix satisfy a given predicate.
dropWhile ::
  Pred a      -- ^ a predicate on list elements
  → InfList a -- ^ an infinite list
  → InfList a -- ^ the rest of the list after satisfying elements are dropped
dropWhile p (x:::xs)
  | p x       = dropWhile p xs
  | otherwise = x:::xs

-- | Given a natural number, return that many elements from the start of an infinite list.
take ::
  Int         -- ^ a natural number
  → InfList a -- ^ an infinite list
  → [a]       -- ^ that many elements from the start of the infinite list
take n (x:::xs)
  | n <= 0    = []
  | otherwise = x : take (pred n) xs

-- | Given a natural number, drop that many elements from the start of an infinite list.
drop ::
  Int         -- ^ a natural number
  → InfList a -- ^ an infinite list
  → InfList a -- ^ the rest of the list after dropping that many elements
drop n l@(x:::xs)
  | n <= 0    = l
  | otherwise = drop (pred n) xs

-- | Given a natural number, get a two-tuple of the finite list of that many elements
-- from the start of a given infinite list, and the infinite list that follows them.
splitAt ::
  Int                 -- ^ a natural number
  → InfList a         -- ^ an infinite list
  → ([a], InfList a)  -- ^ the list split at that index
splitAt n l@(x:::xs)
  | n <= 0    = ([], l)
  | otherwise = let (rest, tl) = splitAt (pred n) xs in (x:rest, tl)

-- | Split an infinite list into a longest prefix such that all the elements of it
-- satisfy a given predicate, and the rest of the list following them.
span ::
  Pred a             -- ^ a predicate on list elements
  → InfList a        -- ^ a list of those elements
  → ([a], InfList a) -- ^ the longest satisfying prefix, and the rest
span p l@(x:::xs)
  | p x       = let (rest, tl) = span p xs in (x:rest, tl)
  | otherwise = ([], l)

-- | Split an infinite list into a longest prefix such that all the elements of it
-- do not satisfy a given predicate, and the rest of the list following them.
break ::
  Pred a             -- ^ a predicate on list elements
  → InfList a        -- ^ a list of those elements
  → ([a], InfList a) -- ^ the longest non-satisfying prefix, and the rest
break p = span (not . p)

-- | Expand all elements of an infinite list into finite lists and concatenate them.
concatMap ::
  (a → [b])   -- ^ the element expanding function
  → InfList a -- ^ a list of those elements
  → InfList b -- ^ the concatenation of the expansion of each element
concatMap m (x:::xs) = m x +++ concatMap m xs

-- | Concatenate an infinite list of finite lists into an infinite list of
-- elements. That is, remove one level of nesting.
concat ::
  InfList [a] -- ^ The infinite list of finite lists
  → InfList a -- ^ The concatenated list
concat (l:::ls) = l +++ concat ls

-- | Given a natural number, get that the element at that index of a given
-- infinite list.
(!!!) ::
  InfList a -- ^ An infinite list of elements
  → Int     -- ^ An index into that list
  → a       -- ^ The element at that index
(x:::xs) !!! n
  | n <= 0    = x
  | otherwise = xs !!! pred n

-- | Zip two infinite lists into an infinite list of two-tuples,
-- where `(a,b)` in the nth tuple are the nth elements of the first and second
-- list respectively.
zip ::
  InfList a       -- ^ The first list
  → InfList b     -- ^ The second list
  → InfList (a,b) -- ^ The zipped list
zip (x:::xs) (y:::ys) = (x,y) ::: zip xs ys

zip3 ::
  InfList a 
  → InfList b 
  → InfList c 
  → InfList (a,b,c)
zip3 (x:::xs) (y:::ys) (z:::zs) = (x,y,z) ::: zip3 xs ys zs

-- | Zip two infinite lists into a third list where the nth element of the third
-- list is the result of applying a given function to the nth elements of the
-- first and second lists.
zipWith ::
  (a → b → c) -- ^ the combining function
  → InfList a -- ^ the first list
  → InfList b -- ^ the second list
  → InfList c -- ^ the result list
zipWith f (x:::xs) (y:::ys) = f x y ::: zipWith f xs ys

zipWith3 ::
  (a → b → c → d) 
  → InfList a 
  → InfList b 
  → InfList c 
  → InfList d
zipWith3 f (x:::xs) (y:::ys) (z:::zs) = f x y z ::: zipWith3 f xs ys zs

-- | Reverse the operation of `zip`.  That is, given an infinite list of
-- two-tuples, return a two-tuple of infinite lists where the nth element
-- of the first list in the result is the first element of the nth tuple of the
-- first list, and correspondingly for the second list in the result tuple.
unzip ::
  InfList (a,b)            -- ^ The infinite list of tuples
  → (InfList a, InfList b) -- ^ The tuple of infinite lists
unzip ((x,y):::rest) = (x:::restx, y:::resty) where (restx, resty) = unzip rest

unzip3 ::
  InfList (a,b,c) 
  → (InfList a, InfList b, InfList c)
unzip3 ((x,y,z):::rest) = (x:::restx, y:::resty, z:::restz)
  where (restx, resty, restz) = unzip3 rest

-- | Given a potential prefix of an infinite list, if the infinite list has
-- that prefix, return just the rest of the infinite list; otherwise,
-- return 'Nothing'.
stripPrefix ::
  Eq a ⇒ [a]          -- ^ the potential prefix
  → InfList a         -- ^ the list to check the prefix of
  → Maybe (InfList a) -- ^ the rest of the list
stripPrefix []     l        = Just l
stripPrefix (x:xs) (y:::ys)
  | x == y    = stripPrefix xs ys
  | otherwise = Nothing

-- | Get the indices into an infinite list where each element at that index
-- satisfies a given predicate.
findIndices ::
  Pred a        -- ^ a predicate on list elements
  → InfList a   -- ^ a list of those elements
  → InfList Int -- ^ a list of indices of satisfying elements
findIndices p l = findIndices' l 0 where
  findIndices' (x:::xs) n
    | p x       = n:::rest
    | otherwise = rest
    where rest = findIndices' xs (succ n)

-- | Get the indices into an infinite list where each element at that index
-- is equal to the given element.
elemIndices :: Eq a ⇒
  a             -- ^ An element to compare to
  → InfList a   -- ^ An infinite list of elements
  → InfList Int -- ^ The indices into that list where the element is equal to the given element
elemIndices x = findIndices (==x)

-- | Is a given finite list a prefix of a given infinite list?
isPrefixOf :: Eq a ⇒
  [a]         -- ^ A finite list
  → InfList a -- ^ An infinite list
  → Bool      -- ^ Whether the finite list is a prefix of the infinite list
[]     `isPrefixOf` _ = True
(x:xs) `isPrefixOf` (y:::ys)
  | x == y = xs `isPrefixOf` ys
  | otherwise = False

-- | Delete the first element satisfying a given predicate.
-- Notice that this is not the same as 'Data.List.deleteBy'.
deleteBy :: (Eq a) ⇒
  Pred a      -- ^ A predicate on list elements
  → InfList a -- ^ A list of those elements
  → InfList a -- ^ The same list minus the first element satisfying the predicate
deleteBy f (x:::xs)
  | f x       = xs
  | otherwise = x ::: deleteBy f xs

-- | Delete the first element equal to a given element.
delete :: (Eq a) ⇒
  a           -- ^ An element to compare to
  → InfList a -- ^ A list
  → InfList a -- ^ The same list minus the first element equal to the given element
delete x = deleteBy (== x)

-- | Place a given element between every adjacent two elements of a given
-- infinite list.
intersperse ::
  a           -- ^ The element to place between
  → InfList a -- ^ The list to intersperse
  → InfList a -- ^ The interspersed list
intersperse sep (x:::xs) = x ::: prependToAll sep xs

-- Not exported.
prependToAll ::
  a 
  → InfList a 
  → InfList a
prependToAll sep (x:::xs) = sep ::: x ::: prependToAll sep xs

-- | Given an infinite list of finite lists, intersperse the infinite list with
-- a given finite list, and concatenate it.
intercalate ::
  [a]           -- ^ The separating list
  → InfList [a] -- ^ The infinite list to intercalate
  → InfList a   -- ^ The infinite list intercalated with the separating list
intercalate xs ls = concat (intersperse xs ls)

-- | Given a predicate and an infinite list, return a pair of
-- infinite lists of elements which do and do not satisfy the
-- predicate, respectively.
partition ::
  Pred a                   -- ^ A predicate on elements
  → InfList a              -- ^ An infinite list of those elements
  → (InfList a, InfList a) -- ^ Those elements satisfying and not satisfying the predicate, respectively
partition p (x:::xs)
  | p x       = (x:::yes, no)
  | otherwise = (yes, x:::no)
  where (yes, no) = partition p xs

-- | Applies a function to each element of a list, passing an accumulating
-- parameter from left to right, returning a new list.
-- This is not quite the same as 'Data.List.mapAccumL': there is no final
-- accumulator returned.
mapAccumL ::
  (acc → a → (acc,b)) -- ^ The mapping/accumulating function
  → acc               -- ^ The initial accumulator
  → InfList a         -- ^ An infinite list
  → InfList b         -- ^ The mapped list
mapAccumL f s (x:::xs) = y ::: mapAccumL f s' xs where (s', y) = f s x

-- mapAccumR?

-- | Given a predicate over two elements, split an infinite list of elements
-- of that type into maximal-length finite lists where each adjacent pair of
-- elements in each list satisfies the predicate in that order.
--
-- In practise, the predicate is associative and commutative, e.g. an equality
-- relation, as used in the 'group' function.
groupBy ::
  (a → a → Bool) -- ^ a binary predicate on elements
  → InfList a    -- ^ an infinite list of that element type
  → InfList [a]  -- ^ maximal-length finite lists satisfying the predicate
groupBy eq (x:::tl) = (x:xs) ::: groupBy eq rest
                      where (xs, rest) = span (eq x) tl

-- | Given an infinite list, split it into maximal-length finite lists where
-- every element in each list is equal.
group :: Eq a ⇒
  InfList a     -- ^ The list of elements
  → InfList [a] -- ^ The list of groups of equal elements
group = groupBy (==)

-- | List the prefixes of an infinite list in ascending order of length.
inits ::
  InfList a     -- ^ A list to take prefixes of
  → InfList [a] -- ^ A list of prefixes in ascending order of length
inits (x:::xs) = [] ::: map (x:) (inits xs)

-- | List the tails of an infinite list in ascending order of distance from
-- the head of the list.
tails ::
  InfList a             -- ^ A list to take tails of
  → InfList (InfList a) -- ^ A list of tails of that list
tails l@(_:::xs) = l ::: tails xs

-- | Build a list from a seed value.  The iterating function takes
-- the seed value and produces the next element in the list along with
-- a new seed value to use for the next element.
-- Not the same as 'Data.List.unfoldr'.
unfoldr ::
  (a → (b,a)) -- ^ The unfolding function
  → a         -- ^ The seed value
  → InfList b -- ^ The unfolded list
unfoldr f base = el ::: unfoldr f nxt where (el,nxt) = f base
