-- | Per-shape styling for diagrams.
-- |
-- | This module provides a Style type for specifying fill, stroke, and other
-- | visual properties, along with combinators for applying styles to diagrams.
module Diagrams.TwoD.Style
  ( -- * Style type
    Style
  , emptyStyle
  , defaultStyle

  -- * Style combinators
  , fc        -- fill color
  , lc        -- line (stroke) color
  , lw        -- line width
  , opacity

  -- * Style merging
  , mergeStyles
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)

-- | Style attributes for shapes.
type Style =
  { fill :: Maybe String
  , stroke :: Maybe String
  , strokeWidth :: Maybe Number
  , fillOpacity :: Maybe Number
  }

-- | Empty style with no attributes set.
emptyStyle :: Style
emptyStyle =
  { fill: Nothing
  , stroke: Nothing
  , strokeWidth: Nothing
  , fillOpacity: Nothing
  }

-- | Default style: black stroke, no fill, 1px stroke width.
defaultStyle :: Style
defaultStyle =
  { fill: Just "none"
  , stroke: Just "black"
  , strokeWidth: Just 1.0
  , fillOpacity: Just 1.0
  }

-- | Set the fill color.
fc :: String -> Style
fc color = emptyStyle { fill = Just color }

-- | Set the stroke (line) color.
lc :: String -> Style
lc color = emptyStyle { stroke = Just color }

-- | Set the stroke (line) width.
lw :: Number -> Style
lw width = emptyStyle { strokeWidth = Just width }

-- | Set the fill opacity.
opacity :: Number -> Style
opacity o = emptyStyle { fillOpacity = Just o }

-- | Merge two styles, with the child's values taking precedence over parent's.
mergeStyles :: Style -> Style -> Style
mergeStyles parent child =
  { fill: alt child.fill parent.fill
  , stroke: alt child.stroke parent.stroke
  , strokeWidth: alt child.strokeWidth parent.strokeWidth
  , fillOpacity: alt child.fillOpacity parent.fillOpacity
  }
  where
  -- Prefer first if Just, otherwise second
  alt :: forall a. Maybe a -> Maybe a -> Maybe a
  alt (Just x) _ = Just x
  alt Nothing y = y
