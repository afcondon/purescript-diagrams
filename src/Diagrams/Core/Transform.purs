-- | Generic transformations parameterized by any vector space.
-- |
-- | This module defines invertible linear transformations and
-- | general affine transformations, along with the Transformable typeclass.
module Diagrams.Core.Transform
  ( -- * Invertible linear transformations
    InvLinear(..)
  , mkInvLinear
  , linv
  , lapp

  -- * General transformations
  , Transformation(..)
  , inv
  , transp
  , transl
  , dropTransl
  , apply
  , papply
  , fromLinear
  , fromOrthogonal
  , fromSymmetric

  -- * The Transformable class
  , class Transformable
  , transform

  -- * Vector space independent transformations
  , translation
  , translate
  , scaling
  , scale
  ) where

import Prelude

import Linear.Affine (Point(..), origin, (.-.), (.+^))
import Linear.Vector (class Additive, zero, negated, (^+^), (^*), (^/))
import Diagrams.Core.HasOrigin (class HasOrigin, moveOriginTo)

-- | An invertible linear transformation, represented as a pair of
-- | mutually inverse functions.
-- |
-- | For `InvLinear f g`, we require that `f . g = g . f = identity`.
data InvLinear u v = InvLinear (u -> v) (v -> u)

-- | Create an invertible linear map from two functions which are
-- | assumed to be linear inverses.
mkInvLinear :: forall u v. (u -> v) -> (v -> u) -> InvLinear u v
mkInvLinear = InvLinear

-- | Invert a linear map.
linv :: forall u v. InvLinear u v -> InvLinear v u
linv (InvLinear f g) = InvLinear g f

-- | Apply a linear map to a vector.
lapp :: forall u v. InvLinear u v -> u -> v
lapp (InvLinear f _) = f

instance Semigroup (InvLinear a a) where
  append (InvLinear f f') (InvLinear g g') = InvLinear (f <<< g) (g' <<< f')

instance Monoid (InvLinear v v) where
  mempty = InvLinear identity identity

-- | General (affine) transformations, represented by an invertible
-- | linear map, its transpose, and a translation vector.
-- |
-- | The reason we need to keep track of transposes is that when
-- | transforming a shape according to some linear map L, the shape's
-- | normal vectors transform according to L's inverse transpose.
-- | This is exactly what we need when transforming envelopes.
data Transformation v n = Transformation (InvLinear (v n) (v n)) (InvLinear (v n) (v n)) (v n)

-- | Invert a transformation.
inv :: forall v n. Functor v => Ring n => Transformation v n -> Transformation v n
inv (Transformation t t' v) = Transformation (linv t) (linv t') (negated (lapp (linv t) v))

-- | Get the transpose of a transformation (ignoring the translation).
transp :: forall v n. Transformation v n -> InvLinear (v n) (v n)
transp (Transformation _ t' _) = t'

-- | Get the translational component of a transformation.
transl :: forall v n. Transformation v n -> v n
transl (Transformation _ _ v) = v

-- | Drop the translational component, leaving only the linear part.
dropTransl :: forall v n. Additive v => Semiring n => Transformation v n -> Transformation v n
dropTransl (Transformation a a' _) = Transformation a a' zero

instance (Additive v, Semiring n) => Semigroup (Transformation v n) where
  append (Transformation t1 t1' v1) (Transformation t2 t2' v2) =
    Transformation (t1 <> t2) (t2' <> t1') (v1 ^+^ lapp t1 v2)

instance (Additive v, Semiring n) => Monoid (Transformation v n) where
  mempty = Transformation mempty mempty zero

-- | Apply a transformation to a vector. The translational component
-- | does not affect vectors (vectors are translation-invariant).
apply :: forall v n. Transformation v n -> v n -> v n
apply (Transformation (InvLinear t _) _ _) = t

-- | Apply a transformation to a point.
papply :: forall v n. Additive v => Semiring n => Transformation v n -> Point v n -> Point v n
papply (Transformation t _ v) (P p) = P (lapp t p ^+^ v)

-- | Create an affine transformation from an invertible linear
-- | transformation and its transpose, with zero translation.
fromLinear :: forall v n. Additive v => Semiring n => InvLinear (v n) (v n) -> InvLinear (v n) (v n) -> Transformation v n
fromLinear l1 l2 = Transformation l1 l2 zero

-- | An orthogonal linear map is one whose inverse is also its transpose.
fromOrthogonal :: forall v n. Additive v => Semiring n => InvLinear (v n) (v n) -> Transformation v n
fromOrthogonal t = fromLinear t (linv t)

-- | A symmetric linear map is one whose transpose equals itself.
fromSymmetric :: forall v n. Additive v => Semiring n => InvLinear (v n) (v n) -> Transformation v n
fromSymmetric t = fromLinear t t

-- | Type class for things which can be transformed.
class Transformable v n t | t -> v n where
  -- | Apply a transformation to an object.
  transform :: Transformation v n -> t -> t

instance (Additive v, Semiring n) => Transformable v n (Transformation v n) where
  transform t1 t2 = t1 <> t2

instance (Applicative v, Additive v, Ring n) => HasOrigin v n (Transformation v n) where
  moveOriginTo p = translate (origin .-. p)

instance (Additive v, Semiring n) => Transformable v n (Point v n) where
  transform = papply

-- | Create a translation.
translation :: forall v n. v n -> Transformation v n
translation = Transformation mempty mempty

-- | Translate by a vector.
translate :: forall v n t. Transformable v n t => v n -> t -> t
translate = transform <<< translation

-- | Create a uniform scaling transformation.
scaling :: forall v n. Additive v => EuclideanRing n => n -> Transformation v n
scaling s = fromSymmetric lin
  where
  lin = mkInvLinear (_ ^* s) (_ ^/ s)

-- | Scale uniformly in every dimension by the given scalar.
scale :: forall v n t. Transformable v n t => Additive v => EuclideanRing n => n -> t -> t
scale s = transform (scaling s)
