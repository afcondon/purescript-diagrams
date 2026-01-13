-- | Envelopes: functional bounding regions.
-- |
-- | Every diagram comes equipped with an envelope. An envelope is an
-- | extensional representation of a bounding region - instead of storing
-- | a direct representation, we store a function which takes a direction
-- | as input and gives a distance to a bounding half-plane as output.
-- |
-- | Envelopes can be composed and transformed by any affine transformation.
module Diagrams.Core.Envelope
  ( Envelope(..)
  , appEnvelope
  , onEnvelope
  , mkEnvelope
  , pointEnvelope

  , class Enveloped
  , getEnvelope

  -- * Utility functions
  , diameter
  , radius
  , extent
  , envelopeVMay
  , envelopeV
  , envelopePMay
  , envelopeP
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord.Max (Max(..))
import Data.Newtype (unwrap)

import Linear.Affine (Point(..))
import Linear.Metric (class Metric, dot, norm, signorm)
import Linear.Vector (class Additive, negated, (*^), (^/))
import Linear.Vector as Vec

import Diagrams.Core.HasOrigin (class HasOrigin)
import Diagrams.Core.Transform (class Transformable, Transformation, apply, inv, lapp, transp, transl)

-- | An envelope is a function that, given a direction vector, returns
-- | the signed distance to a bounding hyperplane perpendicular to that
-- | direction. The `Maybe` handles the empty envelope case.
-- |
-- | Formally, given a vector `v`, the envelope computes a scalar `s` such that:
-- | - for every point `u` inside the diagram, the projection of `u - origin`
-- |   onto `v` is at most `s *^ v`
-- | - `s` is the smallest such scalar
newtype Envelope v n = Envelope (Maybe (v n -> Max n))

-- | Apply an envelope by turning it into a function.
-- | Returns `Nothing` for the empty envelope.
appEnvelope :: forall v n. Envelope v n -> Maybe (v n -> n)
appEnvelope (Envelope e) = map (\f -> unwrap <<< f) e

-- | Transform an envelope by specifying a transformation on the
-- | underlying `v n -> n` function. The empty envelope is unaffected.
onEnvelope :: forall v n. ((v n -> n) -> v n -> n) -> Envelope v n -> Envelope v n
onEnvelope f (Envelope me) = Envelope (map (\g -> Max <<< f (unwrap <<< g)) me)

-- | Create an envelope from a `v n -> n` function.
mkEnvelope :: forall v n. (v n -> n) -> Envelope v n
mkEnvelope f = Envelope (Just (Max <<< f))

-- | Create a point envelope for the given point. A point envelope
-- | has distance zero to a bounding hyperplane in every direction.
-- | Note this is not the same as the empty envelope.
pointEnvelope :: forall v n. Metric v => EuclideanRing n => Point v n -> Envelope v n
pointEnvelope (P p) = onEnvelope (\f v -> f v - ((p ^/ (dot v v)) `dot` v)) (mkEnvelope (const zero))

-- Semigroup/Monoid instances

instance Ord n => Semigroup (Envelope v n) where
  append (Envelope Nothing) e2 = e2
  append e1 (Envelope Nothing) = e1
  append (Envelope (Just f1)) (Envelope (Just f2)) =
    Envelope (Just (\v -> f1 v <> f2 v))

instance Ord n => Monoid (Envelope v n) where
  mempty = Envelope Nothing

-- HasOrigin and Transformable instances

instance (Metric v, EuclideanRing n) => HasOrigin v n (Envelope v n) where
  moveOriginTo (P u) = onEnvelope $ \oldEnv v ->
    oldEnv v - ((u ^/ (dot v v)) `dot` v)

-- Note: The Transformable instance requires Number because signorm is specialized to Number
instance Metric v => Transformable v Number (Envelope v Number) where
  transform t = moveOriginTo' (P (negated (transl t))) <<< onEnvelope g
    where
    g f v = f v' / (dot v' vi)
      where
      v' = signorm (lapp (transp t) v)
      vi = apply (inv t) v
    -- Local helper to avoid import cycle
    moveOriginTo' :: Point v Number -> Envelope v Number -> Envelope v Number
    moveOriginTo' (P u) = onEnvelope $ \oldEnv v ->
      oldEnv v - ((u ^/ (dot v v)) `dot` v)

-- | Class for things which have an envelope.
class (Metric v, Ord n) <= Enveloped v n a | a -> v n where
  getEnvelope :: a -> Envelope v n

instance (Metric v, Ord n) => Enveloped v n (Envelope v n) where
  getEnvelope = identity

instance (Metric v, Ord n, EuclideanRing n) => Enveloped v n (Point v n) where
  getEnvelope p = pointEnvelope p

-- | Compute the vector from the local origin to a separating hyperplane
-- | in the given direction, or `Nothing` for the empty envelope.
envelopeVMay :: forall v n a. Enveloped v n a => Semiring n => v n -> a -> Maybe (v n)
envelopeVMay v a = case appEnvelope (getEnvelope a) of
  Nothing -> Nothing
  Just f -> Just ((f v) *^ v)

-- | Compute the vector from the local origin to a separating hyperplane
-- | in the given direction. Returns the zero vector for the empty envelope.
envelopeV :: forall v n a. Enveloped v n a => Semiring n => v n -> a -> v n
envelopeV v a = fromMaybe Vec.zero (envelopeVMay v a)

-- | Compute the point on a separating hyperplane in the given direction,
-- | or `Nothing` for the empty envelope.
envelopePMay :: forall v n a. Enveloped v n a => Semiring n => v n -> a -> Maybe (Point v n)
envelopePMay v a = P <$> envelopeVMay v a

-- | Compute the point on a separating hyperplane in the given direction.
-- | Returns the origin for the empty envelope.
envelopeP :: forall v n a. Enveloped v n a => Semiring n => v n -> a -> Point v n
envelopeP v a = P (envelopeV v a)

-- | Compute the range of an enveloped object along a direction.
-- | Returns `(lo, hi)` such that the object extends from `lo *^ v` to `hi *^ v`.
extent :: forall v n a. Enveloped v n a => Ring n => v n -> a -> Maybe { lo :: n, hi :: n }
extent v a = case appEnvelope (getEnvelope a) of
  Nothing -> Nothing
  Just f -> Just { lo: negate (f (negated v)), hi: f v }

-- | Compute the diameter of an enveloped object along a direction.
-- | Returns zero for the empty envelope.
-- | Note: specialized to Number because norm returns Number
diameter :: forall v a. Enveloped v Number a => v Number -> a -> Number
diameter v a = case extent v a of
  Nothing -> zero
  Just { lo, hi } -> (hi - lo) * norm v

-- | Compute the radius (half the diameter) of an enveloped object.
radius :: forall v a. Enveloped v Number a => v Number -> a -> Number
radius v a = diameter v a / 2.0
