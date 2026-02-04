-- | SVG backend for diagrams.
-- |
-- | Renders Diagram2D to SVG strings, with support for per-shape styling.
module Diagrams.Backend.SVG
  ( renderSVG
  , renderSVGWith
  , SVGOptions
  , defaultSVGOptions
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (intercalate)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (toStringWith, fixed)

import Linear.V2 (V2(..))

import Diagrams.TwoD.Diagram (Diagram2D(..), width, height, envelope)
import Diagrams.TwoD.Shapes (Shape2D(..))
import Diagrams.TwoD.Style (Style, defaultStyle, mergeStyles)
import Diagrams.TwoD.Types (unitX, unitY, unit_X, unit_Y)
import Diagrams.Core.Transform (transl, apply)
import Linear.Metric (norm)

-- | Options for SVG rendering.
type SVGOptions =
  { width :: Number
  , height :: Number
  , strokeColor :: String
  , strokeWidth :: Number
  , fillColor :: String
  , fillOpacity :: Number
  }

-- | Default SVG rendering options.
defaultSVGOptions :: SVGOptions
defaultSVGOptions =
  { width: 400.0
  , height: 400.0
  , strokeColor: "black"
  , strokeWidth: 1.0
  , fillColor: "none"
  , fillOpacity: 1.0
  }

-- Convert SVGOptions to Style for internal use
optionsToStyle :: SVGOptions -> Style
optionsToStyle opts =
  { fill: Just opts.fillColor
  , stroke: Just opts.strokeColor
  , strokeWidth: Just opts.strokeWidth
  , fillOpacity: Just opts.fillOpacity
  }

-- | Render a diagram to an SVG string with default options.
renderSVG :: Diagram2D -> String
renderSVG = renderSVGWith defaultSVGOptions

-- | Render a diagram to an SVG string with custom options.
renderSVGWith :: SVGOptions -> Diagram2D -> String
renderSVGWith opts diagram =
  let -- Calculate diagram extents in each direction
      w = opts.width
      h = opts.height
      -- Get extent in each direction (envelope returns distance from origin)
      extentPosX = fromMaybe 0.0 (envelope unitX diagram)
      extentNegX = fromMaybe 0.0 (envelope unit_X diagram)
      extentPosY = fromMaybe 0.0 (envelope unitY diagram)
      extentNegY = fromMaybe 0.0 (envelope unit_Y diagram)
      -- Total dimensions
      diagramW = extentPosX + extentNegX
      diagramH = extentPosY + extentNegY
      -- Calculate offset to center the diagram
      -- The diagram's center is at ((extentPosX - extentNegX)/2, (extentPosY - extentNegY)/2)
      -- We need to shift by the negative of this to center at origin
      offsetX = (extentNegX - extentPosX) / 2.0
      offsetY = (extentNegY - extentPosY) / 2.0
      -- Scale to fit viewport with 10% padding
      scaleF = min (w / max 1.0 diagramW) (h / max 1.0 diagramH) * 0.9
      baseStyle = optionsToStyle opts
      -- Render at viewport center, with offset to center the diagram
      cx = w / 2.0 + offsetX * scaleF
      cy = h / 2.0 - offsetY * scaleF  -- Y is flipped in SVG
      content = renderDiagram baseStyle scaleF cx cy diagram
  in svgHeader opts <> content <> svgFooter

svgHeader :: SVGOptions -> String
svgHeader opts =
  "<svg xmlns=\"http://www.w3.org/2000/svg\" " <>
  "width=\"" <> num opts.width <> "\" " <>
  "height=\"" <> num opts.height <> "\" " <>
  "viewBox=\"0 0 " <> num opts.width <> " " <> num opts.height <> "\">\n"

svgFooter :: String
svgFooter = "</svg>\n"

-- Render diagram recursively, tracking accumulated transform and style
renderDiagram :: Style -> Number -> Number -> Number -> Diagram2D -> String
renderDiagram _ _ _ _ Empty = ""
renderDiagram style scale cx cy (Prim shape) =
  renderShape style scale cx cy shape
renderDiagram style scale cx cy (Transformed t d) =
  -- Extract both translation and scale from the transformation
  -- Scale factor: apply transform to unit vector and measure length
  let V2 tx ty = transl t
      localScale = norm (apply t unitX)  -- How much does the transform scale?
      newScale = scale * localScale
  in renderDiagram style newScale (cx + tx * scale) (cy - ty * scale) d
renderDiagram style scale cx cy (Compose ds) =
  List.foldl (\acc d -> acc <> renderDiagram style scale cx cy d) "" ds
renderDiagram style scale cx cy (Styled childStyle d) =
  -- Merge styles: child style overrides parent style
  let mergedStyle = mergeStyles style childStyle
  in renderDiagram mergedStyle scale cx cy d

-- Render a primitive shape
renderShape :: Style -> Number -> Number -> Number -> Shape2D -> String
renderShape style scale cx cy (Circle r) =
  "  <circle " <>
  "cx=\"" <> num cx <> "\" " <>
  "cy=\"" <> num cy <> "\" " <>
  "r=\"" <> num (r * scale) <> "\" " <>
  styleAttrs style <>
  "/>\n"

renderShape style scale cx cy (Rectangle w h) =
  let x = cx - (w * scale / 2.0)
      y = cy - (h * scale / 2.0)
  in "  <rect " <>
     "x=\"" <> num x <> "\" " <>
     "y=\"" <> num y <> "\" " <>
     "width=\"" <> num (w * scale) <> "\" " <>
     "height=\"" <> num (h * scale) <> "\" " <>
     styleAttrs style <>
     "/>\n"

renderShape style scale cx cy (LineSegment (V2 vx vy)) =
  let x1 = cx - (vx * scale / 2.0)
      y1 = cy + (vy * scale / 2.0)  -- Y is flipped in SVG
      x2 = cx + (vx * scale / 2.0)
      y2 = cy - (vy * scale / 2.0)
  in "  <line " <>
     "x1=\"" <> num x1 <> "\" " <>
     "y1=\"" <> num y1 <> "\" " <>
     "x2=\"" <> num x2 <> "\" " <>
     "y2=\"" <> num y2 <> "\" " <>
     lineStyleAttrs style <>
     "/>\n"

renderShape style scale cx cy (Polygon vertices) =
  let points = vertices # map \(V2 vx vy) ->
        num (cx + vx * scale) <> "," <> num (cy - vy * scale)
      pointsStr = intercalate " " points
  in "  <polygon " <>
     "points=\"" <> pointsStr <> "\" " <>
     styleAttrs style <>
     "/>\n"

styleAttrs :: Style -> String
styleAttrs style =
  "stroke=\"" <> fromMaybe "black" style.stroke <> "\" " <>
  "stroke-width=\"" <> num (fromMaybe 1.0 style.strokeWidth) <> "\" " <>
  "fill=\"" <> fromMaybe "none" style.fill <> "\" " <>
  "fill-opacity=\"" <> num (fromMaybe 1.0 style.fillOpacity) <> "\""

lineStyleAttrs :: Style -> String
lineStyleAttrs style =
  "stroke=\"" <> fromMaybe "black" style.stroke <> "\" " <>
  "stroke-width=\"" <> num (fromMaybe 1.0 style.strokeWidth) <> "\""

-- Format a number for SVG
num :: Number -> String
num n = toStringWith (fixed 2) n
