-- | Traces: functional boundaries via ray-casting.
-- |
-- | Every diagram can have a trace. Intuitively, the trace for a diagram
-- | is like a raytracer: given a line (represented as a base point and a
-- | direction vector), the trace computes a sorted list of signed distances
-- | from the base point to all intersections of the line with the boundary.
module Diagrams.Core.Trace
  ( -- * SortedList
    SortedList
  , mkSortedList
  , getSortedList
  , onSortedList

  -- * Traces
  , Trace(..)
  , appTrace
  , mkTrace

  -- * Traced class
  , class Traced
  , getTrace

  -- * Computing with traces
  , traceV
  , traceP
  , maxTraceV
  , maxTraceP
  , rayTraceV
  , rayTraceP
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.List as List

import Linear.Affine (Point(..), (.+^))
import Linear.Vector (class Additive, negated, (*^), (^*))
import Linear.Vector as Vec

import Diagrams.Core.HasOrigin (class HasOrigin)
import Diagrams.Core.Transform (class Transformable, Transformation, apply, inv, papply)

-- | A sorted list, maintaining the invariant that elements are in order.
newtype SortedList a = SortedList (List a)

-- | Create a sorted list by sorting the input.
mkSortedList :: forall a. Ord a => List a -> SortedList a
mkSortedList = SortedList <<< List.sort

-- | Get the underlying sorted list.
getSortedList :: forall a. SortedList a -> List a
getSortedList (SortedList xs) = xs

-- | Apply an order-preserving function to a sorted list.
onSortedList :: forall a b. Ord b => (List a -> List b) -> SortedList a -> SortedList b
onSortedList f (SortedList xs) = mkSortedList (f xs)

-- Merge two sorted lists
merge :: forall a. Ord a => SortedList a -> SortedList a -> SortedList a
merge (SortedList xs) (SortedList ys) = SortedList (mergeList xs ys)
  where
  mergeList List.Nil bs = bs
  mergeList as List.Nil = as
  mergeList (List.Cons a as) (List.Cons b bs)
    | a <= b = List.Cons a (mergeList as (List.Cons b bs))
    | otherwise = List.Cons b (mergeList (List.Cons a as) bs)

instance Ord a => Semigroup (SortedList a) where
  append = merge

instance Ord a => Monoid (SortedList a) where
  mempty = SortedList List.Nil

-- | A trace is a function that, given a base point and direction vector,
-- | returns a sorted list of signed distances to all intersections with
-- | the diagram's boundary.
-- |
-- | The outputs are multipliers relative to the input vector: if the base
-- | point is `p` and direction is `v`, and one of the output scalars is `s`,
-- | then there is an intersection at point `p .+^ (s *^ v)`.
newtype Trace v n = Trace (Point v n -> v n -> SortedList n)

-- | Apply a trace to get the intersection distances.
appTrace :: forall v n. Trace v n -> Point v n -> v n -> SortedList n
appTrace (Trace f) = f

-- | Create a trace from a function.
mkTrace :: forall v n. (Point v n -> v n -> SortedList n) -> Trace v n
mkTrace = Trace

instance Ord n => Semigroup (Trace v n) where
  append (Trace f1) (Trace f2) = Trace \p v -> f1 p v <> f2 p v

instance Ord n => Monoid (Trace v n) where
  mempty = Trace \_ _ -> mempty

instance (Additive v, Semiring n) => HasOrigin v n (Trace v n) where
  moveOriginTo (P u) (Trace f) = Trace \p v -> f (p .+^ u) v

instance (Additive v, Ring n) => Transformable v n (Trace v n) where
  transform t (Trace f) = Trace \p v ->
    f (papply (inv t) p) (apply (inv t) v)

-- | Class for things which have a trace.
class (Additive v, Ord n) <= Traced v n a | a -> v n where
  getTrace :: a -> Trace v n

instance (Additive v, Ord n) => Traced v n (Trace v n) where
  getTrace = identity

-- The trace of a single point is the empty trace.
instance (Additive v, Ord n) => Traced v n (Point v n) where
  getTrace _ = mempty

-- | Compute the vector from the given point to the smallest boundary
-- | intersection along the given direction. Returns `Nothing` if there
-- | is no intersection.
traceV :: forall v n a. Traced v n a => Semiring n => Point v n -> v n -> a -> Maybe (v n)
traceV p v a = case List.head (getSortedList (appTrace (getTrace a) p v)) of
  Nothing -> Nothing
  Just s -> Just (s *^ v)

-- | Compute the smallest boundary point along the line.
traceP :: forall v n a. Traced v n a => Semiring n => Point v n -> v n -> a -> Maybe (Point v n)
traceP p v a = map (p .+^ _) (traceV p v a)

-- | Compute the vector to the largest boundary point.
maxTraceV :: forall v n a. Traced v n a => Ring n => Point v n -> v n -> a -> Maybe (v n)
maxTraceV p v = traceV p (negated v)

-- | Compute the largest boundary point.
maxTraceP :: forall v n a. Traced v n a => Ring n => Point v n -> v n -> a -> Maybe (Point v n)
maxTraceP p v a = map (p .+^ _) (maxTraceV p v a)

-- | Like `traceV`, but only considers positive intersections (in front of
-- | the base point in the direction of the vector).
rayTraceV :: forall v n a. Traced v n a => Ord n => Semiring n => Point v n -> v n -> a -> Maybe (v n)
rayTraceV p v a =
  let intersections = getSortedList (appTrace (getTrace a) p v)
      positive = List.filter (_ >= zero) intersections
  in case List.head positive of
    Nothing -> Nothing
    Just s -> Just (s *^ v)

-- | Like `traceP`, but only considers positive intersections.
rayTraceP :: forall v n a. Traced v n a => Ord n => Semiring n => Point v n -> v n -> a -> Maybe (Point v n)
rayTraceP p v a = map (p .+^ _) (rayTraceV p v a)
