-- | Types which have an intrinsic notion of a "local origin",
-- | i.e. things which are not invariant under translation.
module Diagrams.Core.HasOrigin
  ( class HasOrigin
  , moveOriginTo
  , moveOriginBy
  , moveTo
  , place
  ) where

import Prelude

import Linear.Affine (class Affine, Point(..), origin, (.-.), (.-^))
import Linear.Vector (class Additive)

-- | Class of types which have an intrinsic notion of a "local origin",
-- | i.e. things which are not invariant under translation, and which
-- | allow the origin to be moved.
-- |
-- | For types which are also `Transformable`, we should have:
-- | ```
-- | moveOriginTo (origin .+^ v) === translate (negated v)
-- | ```
class HasOrigin v n t | t -> v n where
  -- | Move the local origin to another point.
  moveOriginTo :: Point v n -> t -> t

-- | Move the local origin by a relative vector.
moveOriginBy :: forall v n t. HasOrigin v n t => v n -> t -> t
moveOriginBy v = moveOriginTo (P v)

-- | Translate the object by the translation that sends the origin to
-- | the given point.
-- |
-- | For types which are also `Transformable`, this is essentially the
-- | same as `translate`:
-- | ```
-- | moveTo (origin .+^ v) === translate v
-- | ```
moveTo :: forall v n t. Applicative v => Additive v => Ring n => HasOrigin v n t => Point v n -> t -> t
moveTo p = moveOriginBy (origin .-. p)

-- | A flipped variant of `moveTo`.
place :: forall v n t. Applicative v => Additive v => Ring n => HasOrigin v n t => t -> Point v n -> t
place t p = moveTo p t

-- Instances

instance (Additive v, Ring n) => HasOrigin v n (Point v n) where
  moveOriginTo (P u) p = p .-^ u
