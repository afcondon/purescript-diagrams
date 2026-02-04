-- | Layout combinators for two-dimensional diagrams.
-- |
-- | This module provides functions for arranging diagrams in various layouts:
-- | horizontal/vertical concatenation, grids, and sizing utilities.
module Diagrams.TwoD.Combinators
  ( -- * Concatenation
    hcat
  , vcat
  , hsep
  , vsep

  -- * Grid layouts
  , grid
  , gridWithSep

  -- * Sizing and centering
  , centerXY
  , sized
  , ensquare
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))

import Linear.V2 (V2(..))

import Diagrams.TwoD.Diagram (Diagram2D, beside, width, height, scaleD, translateD)
import Diagrams.TwoD.Types (unitX, unitY, unit_X, unit_Y)
import Diagrams.Core.Envelope (diameter)

-- | Concatenate diagrams horizontally (left to right).
hcat :: Array Diagram2D -> Diagram2D
hcat = hsep 0.0

-- | Concatenate diagrams vertically (top to bottom, first element at top).
vcat :: Array Diagram2D -> Diagram2D
vcat = vsep 0.0

-- | Concatenate diagrams horizontally with spacing.
hsep :: Number -> Array Diagram2D -> Diagram2D
hsep spacing ds = case Array.uncons ds of
  Nothing -> mempty
  Just { head, tail } ->
    foldl (\acc d -> beside unitX acc (translateD (V2 spacing 0.0) d)) head tail

-- | Concatenate diagrams vertically with spacing (top to bottom).
vsep :: Number -> Array Diagram2D -> Diagram2D
vsep spacing ds = case Array.uncons ds of
  Nothing -> mempty
  Just { head, tail } ->
    foldl (\acc d -> beside unit_Y acc (translateD (V2 0.0 (negate spacing)) d)) head tail

-- | Arrange diagrams in a grid (array of rows, top row first).
grid :: Array (Array Diagram2D) -> Diagram2D
grid = gridWithSep 0.0 0.0

-- | Arrange diagrams in a grid with horizontal and vertical spacing.
gridWithSep :: Number -> Number -> Array (Array Diagram2D) -> Diagram2D
gridWithSep hspacing vspacing rows =
  vsep vspacing (map (hsep hspacing) rows)

-- | Center a diagram at the origin.
-- | Translates the diagram so its center (based on envelope) is at the origin.
centerXY :: Diagram2D -> Diagram2D
centerXY d =
  let w = width d
      h = height d
      -- Get the extent in each direction
      extentPosX = diameter unitX d / 2.0
      extentNegX = diameter unit_X d / 2.0
      extentPosY = diameter unitY d / 2.0
      extentNegY = diameter unit_Y d / 2.0
      -- Calculate offset to center
      offsetX = (extentNegX - extentPosX) / 2.0
      offsetY = (extentNegY - extentPosY) / 2.0
  in translateD (V2 offsetX offsetY) d

-- | Scale a diagram to fit within given dimensions, preserving aspect ratio.
sized :: Number -> Number -> Diagram2D -> Diagram2D
sized targetW targetH d =
  let w = max 0.001 (width d)
      h = max 0.001 (height d)
      scaleX = targetW / w
      scaleY = targetH / h
      scaleFactor = min scaleX scaleY
  in scaleD scaleFactor d

-- | Fit a diagram into a square of the given size with padding.
-- | The padding is the fraction of the size to leave as margin on each side.
ensquare :: Number -> Diagram2D -> Diagram2D
ensquare padding d =
  let w = width d
      h = height d
      size = max w h
      targetSize = 1.0 - 2.0 * padding
      scaleFactor = if size < 0.001 then 1.0 else targetSize / size
  in centerXY (scaleD scaleFactor d)
