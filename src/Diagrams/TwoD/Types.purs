-- | Basic types for two-dimensional Euclidean space.
module Diagrams.TwoD.Types
  ( -- * Type aliases
    P2
  , V2N
  , T2

  -- * Constructors
  , p2
  , r2
  , mkP2
  , mkR2

  -- * Destructors
  , unp2
  , unr2

  -- * Unit vectors
  , unitX
  , unitY
  , unit_X
  , unit_Y
  ) where

import Prelude

import Linear.V2 (V2(..))
import Linear.Affine (Point(..))
import Diagrams.Core.Transform (Transformation)

-- | 2D point type alias
type P2 = Point V2

-- | 2D vector with Number coordinates
type V2N = V2 Number

-- | 2D transformation type alias
type T2 = Transformation V2

-- | Construct a 2D vector from a pair of components.
r2 :: forall n. { x :: n, y :: n } -> V2 n
r2 { x, y } = V2 x y

-- | Convert a 2D vector to a record.
unr2 :: forall n. V2 n -> { x :: n, y :: n }
unr2 (V2 x y) = { x, y }

-- | Curried form of `r2`.
mkR2 :: forall n. n -> n -> V2 n
mkR2 = V2

-- | Construct a 2D point from a pair of coordinates.
p2 :: forall n. { x :: n, y :: n } -> P2 n
p2 { x, y } = P (V2 x y)

-- | Convert a 2D point to a record.
unp2 :: forall n. P2 n -> { x :: n, y :: n }
unp2 (P (V2 x y)) = { x, y }

-- | Curried form of `p2`.
mkP2 :: forall n. n -> n -> P2 n
mkP2 x y = P (V2 x y)

-- | Unit vector in the positive X direction.
unitX :: V2 Number
unitX = V2 1.0 0.0

-- | Unit vector in the positive Y direction.
unitY :: V2 Number
unitY = V2 0.0 1.0

-- | Unit vector in the negative X direction.
unit_X :: V2 Number
unit_X = V2 (-1.0) 0.0

-- | Unit vector in the negative Y direction.
unit_Y :: V2 Number
unit_Y = V2 0.0 (-1.0)
