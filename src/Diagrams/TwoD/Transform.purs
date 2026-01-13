-- | Transformations specific to two dimensions.
-- |
-- | Provides 2D rotation, scaling, reflection, and translation utilities.
module Diagrams.TwoD.Transform
  ( -- * Rotation
    rotation
  , rotate
  , rotateBy
  , rotationAround
  , rotateAround

  -- * Scaling
  , scalingX
  , scaleX
  , scalingY
  , scaleY

  -- * Translation
  , translationX
  , translateX
  , translationY
  , translateY

  -- * Reflection
  , reflectionX
  , reflectX
  , reflectionY
  , reflectY

  -- * Type alias
  , T2
  ) where

import Prelude

import Data.Number (cos, sin)

import Linear.V2 (V2(..))
import Linear.Affine (Point(..), origin, (.-.))

import Diagrams.Core.Transform
  ( class Transformable
  , Transformation
  , mkInvLinear
  , fromSymmetric
  , fromOrthogonal
  , transform
  , translation
  )

-- | Type alias for 2D transformations.
type T2 = Transformation V2

-- Rotation ------------------------------------------------

-- | Create a rotation transformation by the given angle (in radians).
-- |
-- | Positive angles rotate counter-clockwise.
rotation :: Number -> T2 Number
rotation theta = fromOrthogonal lin
  where
  c = cos theta
  s = sin theta
  lin = mkInvLinear
    (\(V2 x y) -> V2 (c * x - s * y) (s * x + c * y))
    (\(V2 x y) -> V2 (c * x + s * y) (negate s * x + c * y))

-- | Rotate by the given angle (in radians).
rotate :: forall t. Transformable V2 Number t => Number -> t -> t
rotate = transform <<< rotation

-- | Rotate by the given fraction of a full turn.
-- |
-- | `rotateBy 0.25` rotates by 90 degrees counter-clockwise.
rotateBy :: forall t. Transformable V2 Number t => Number -> t -> t
rotateBy turns = rotate (turns * 2.0 * pi)
  where
  pi = 3.141592653589793

-- | Rotation about a specific point (instead of the origin).
rotationAround :: Point V2 Number -> Number -> T2 Number
rotationAround p theta =
  let v = origin .-. p
  in translation v <> rotation theta <> translation (negate <$> v)

-- | Rotate about a specific point by the given angle.
rotateAround :: forall t. Transformable V2 Number t => Point V2 Number -> Number -> t -> t
rotateAround p theta = transform (rotationAround p theta)

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor
-- | in the x (horizontal) direction.
scalingX :: Number -> T2 Number
scalingX c = fromSymmetric $ mkInvLinear
  (\(V2 x y) -> V2 (c * x) y)
  (\(V2 x y) -> V2 (x / c) y)

-- | Scale by the given factor in the x direction.
scaleX :: forall t. Transformable V2 Number t => Number -> t -> t
scaleX = transform <<< scalingX

-- | Construct a transformation which scales by the given factor
-- | in the y (vertical) direction.
scalingY :: Number -> T2 Number
scalingY c = fromSymmetric $ mkInvLinear
  (\(V2 x y) -> V2 x (c * y))
  (\(V2 x y) -> V2 x (y / c))

-- | Scale by the given factor in the y direction.
scaleY :: forall t. Transformable V2 Number t => Number -> t -> t
scaleY = transform <<< scalingY

-- Translation ---------------------------------------------

-- | Construct a transformation which translates by the given distance
-- | in the x (horizontal) direction.
translationX :: Number -> T2 Number
translationX x = translation (V2 x 0.0)

-- | Translate by the given distance in the x direction.
translateX :: forall t. Transformable V2 Number t => Number -> t -> t
translateX = transform <<< translationX

-- | Construct a transformation which translates by the given distance
-- | in the y (vertical) direction.
translationY :: Number -> T2 Number
translationY y = translation (V2 0.0 y)

-- | Translate by the given distance in the y direction.
translateY :: forall t. Transformable V2 Number t => Number -> t -> t
translateY = transform <<< translationY

-- Reflection ----------------------------------------------

-- | Construct a transformation which reflects about the y-axis,
-- | i.e. sends (x,y) to (-x,y).
reflectionX :: T2 Number
reflectionX = fromSymmetric $ mkInvLinear
  (\(V2 x y) -> V2 (negate x) y)
  (\(V2 x y) -> V2 (negate x) y)

-- | Reflect about the y-axis, i.e. flip horizontally.
reflectX :: forall t. Transformable V2 Number t => t -> t
reflectX = transform reflectionX

-- | Construct a transformation which reflects about the x-axis,
-- | i.e. sends (x,y) to (x,-y).
reflectionY :: T2 Number
reflectionY = fromSymmetric $ mkInvLinear
  (\(V2 x y) -> V2 x (negate y))
  (\(V2 x y) -> V2 x (negate y))

-- | Reflect about the x-axis, i.e. flip vertically.
reflectY :: forall t. Transformable V2 Number t => t -> t
reflectY = transform reflectionY
