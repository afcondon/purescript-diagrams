-- | Two-dimensional diagrams.
-- |
-- | This module re-exports the main types and functions for creating
-- | and manipulating 2D diagrams.
module Diagrams.TwoD
  ( -- * Re-exports
    module Diagrams.TwoD.Types
  , module Diagrams.TwoD.Transform
  , module Diagrams.TwoD.Shapes
  , module Diagrams.TwoD.Diagram
  ) where

import Diagrams.TwoD.Types
  ( P2
  , V2N
  , T2
  , p2
  , r2
  , mkP2
  , mkR2
  , unp2
  , unr2
  , unitX
  , unitY
  , unit_X
  , unit_Y
  )

import Diagrams.TwoD.Transform
  ( rotation
  , rotate
  , rotateBy
  , rotationAround
  , rotateAround
  , scalingX
  , scaleX
  , scalingY
  , scaleY
  , translationX
  , translateX
  , translationY
  , translateY
  , reflectionX
  , reflectX
  , reflectionY
  , reflectY
  )

import Diagrams.TwoD.Shapes
  ( Shape2D(..)
  , circle
  , unitCircle
  , rect
  , square
  , unitSquare
  , hrule
  , vrule
  )

import Diagrams.TwoD.Diagram
  ( Diagram2D(..)
  , fromShape
  , empty
  , atop
  , beside
  , above
  , below
  , translateD
  , scaleD
  , rotateD
  , envelope
  , width
  , height
  )
