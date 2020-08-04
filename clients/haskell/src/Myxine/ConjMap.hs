module Myxine.ConjMap
  ( ConjMap
  , empty
  , lookup
  , insert
  , union
  ) where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Hashable
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

data ConjMap k a
  = ConjMap [a] (HashMap k (ConjMap k a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

empty :: ConjMap k a
empty = ConjMap [] HashMap.empty

-- | Retrieve all items whose keys are a (non-strict) subset of the specified
-- keys.
lookup :: (Eq k, Hashable k) => [k] -> ConjMap k a -> [a]
lookup = go . HashSet.fromList
  where
    go facts (ConjMap universal specific) =
      universal <> fromMaybe [] (goSpecific facts specific)

    goSpecific facts specific =
      foldMap (\fact -> go (HashSet.delete fact facts) <$>
                           HashMap.lookup fact specific) facts

-- | Add an item such that it can be retrieved only by giving a (non-strict)
-- superset of all the specified keys.
insert :: (Eq k, Hashable k) => [k] -> a -> ConjMap k a -> ConjMap k a
insert patList a =
  goTree (HashSet.fromList patList)
  where
    -- Invariant: no pattern appears twice in a branch of a tree, although it
    -- can occur multiple times in separate branches
    goTree pats (ConjMap universal specific)
      | HashSet.null pats = ConjMap (a : universal) specific
      | otherwise = ConjMap universal (goNodes pats (HashMap.toList specific))

    goNodes pats [] = freshNode pats
    goNodes pats ((k, t) : rest)
      | HashSet.member k pats =
        HashMap.fromList ((k, goTree (HashSet.delete k pats) t) : rest)
      | otherwise =
        HashMap.insert k t (goNodes pats rest)

    freshNode (HashSet.toList -> pats) = freshNode' pats
      where
        freshNode' []       = error "Internal error: addMatch: [] passed to freshNode"
        freshNode' [p]      = HashMap.singleton p (ConjMap [a] HashMap.empty)
        freshNode' (p : ps) = HashMap.singleton p (ConjMap [] (freshNode' ps))

union :: (Eq k, Hashable k) => ConjMap k a -> ConjMap k a -> ConjMap k a
union (ConjMap u s) (ConjMap u' s') =
  ConjMap (u <> u') (HashMap.unionWith union s s')

instance (Eq k, Hashable k) => Semigroup (ConjMap k a) where
  (<>) = union

instance (Eq k, Hashable k) => Monoid (ConjMap k a) where
  mempty = empty
