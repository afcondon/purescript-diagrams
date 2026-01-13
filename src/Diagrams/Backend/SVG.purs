-- | SVG backend for diagrams.
-- |
-- | Renders Diagram2D to SVG strings.
module Diagrams.Backend.SVG
  ( renderSVG
  , renderSVGWith
  , SVGOptions
  , defaultSVGOptions
  ) where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Number (pi, cos, sin)
import Data.Number.Format (toString, toStringWith, fixed)

import Linear.V2 (V2(..))
import Linear.Affine (Point(..))

import Diagrams.TwoD.Diagram (Diagram2D(..), width, height)
import Diagrams.TwoD.Shapes (Shape2D(..))
import Diagrams.TwoD.Transform (T2)
import Diagrams.Core.Transform (Transformation(..), transl, apply)

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

-- | Render a diagram to an SVG string with default options.
renderSVG :: Diagram2D -> String
renderSVG = renderSVGWith defaultSVGOptions

-- | Render a diagram to an SVG string with custom options.
renderSVGWith :: SVGOptions -> Diagram2D -> String
renderSVGWith opts diagram =
  let -- Center the diagram in the viewport
      w = opts.width
      h = opts.height
      diagramW = width diagram
      diagramH = height diagram
      scaleF = min (w / max 1.0 diagramW) (h / max 1.0 diagramH) * 0.9
      content = renderDiagram opts scaleF (w / 2.0) (h / 2.0) diagram
  in svgHeader opts <> content <> svgFooter

svgHeader :: SVGOptions -> String
svgHeader opts =
  "<svg xmlns=\"http://www.w3.org/2000/svg\" " <>
  "width=\"" <> num opts.width <> "\" " <>
  "height=\"" <> num opts.height <> "\" " <>
  "viewBox=\"0 0 " <> num opts.width <> " " <> num opts.height <> "\">\n"

svgFooter :: String
svgFooter = "</svg>\n"

-- Render diagram recursively, tracking accumulated transform
renderDiagram :: SVGOptions -> Number -> Number -> Number -> Diagram2D -> String
renderDiagram _ _ _ _ Empty = ""
renderDiagram opts scale cx cy (Prim shape) =
  renderShape opts scale cx cy shape
renderDiagram opts scale cx cy (Transformed t d) =
  -- Apply transformation offset
  let V2 tx ty = transl t
  in renderDiagram opts scale (cx + tx * scale) (cy - ty * scale) d
renderDiagram opts scale cx cy (Compose ds) =
  List.foldl (\acc d -> acc <> renderDiagram opts scale cx cy d) "" ds

-- Render a primitive shape
renderShape :: SVGOptions -> Number -> Number -> Number -> Shape2D -> String
renderShape opts scale cx cy (Circle r) =
  "  <circle " <>
  "cx=\"" <> num cx <> "\" " <>
  "cy=\"" <> num cy <> "\" " <>
  "r=\"" <> num (r * scale) <> "\" " <>
  styleAttrs opts <>
  "/>\n"

renderShape opts scale cx cy (Rectangle w h) =
  let x = cx - (w * scale / 2.0)
      y = cy - (h * scale / 2.0)
  in "  <rect " <>
     "x=\"" <> num x <> "\" " <>
     "y=\"" <> num y <> "\" " <>
     "width=\"" <> num (w * scale) <> "\" " <>
     "height=\"" <> num (h * scale) <> "\" " <>
     styleAttrs opts <>
     "/>\n"

renderShape opts scale cx cy (LineSegment (V2 vx vy)) =
  let x1 = cx - (vx * scale / 2.0)
      y1 = cy + (vy * scale / 2.0)  -- Y is flipped in SVG
      x2 = cx + (vx * scale / 2.0)
      y2 = cy - (vy * scale / 2.0)
  in "  <line " <>
     "x1=\"" <> num x1 <> "\" " <>
     "y1=\"" <> num y1 <> "\" " <>
     "x2=\"" <> num x2 <> "\" " <>
     "y2=\"" <> num y2 <> "\" " <>
     lineStyleAttrs opts <>
     "/>\n"

styleAttrs :: SVGOptions -> String
styleAttrs opts =
  "stroke=\"" <> opts.strokeColor <> "\" " <>
  "stroke-width=\"" <> num opts.strokeWidth <> "\" " <>
  "fill=\"" <> opts.fillColor <> "\" " <>
  "fill-opacity=\"" <> num opts.fillOpacity <> "\""

lineStyleAttrs :: SVGOptions -> String
lineStyleAttrs opts =
  "stroke=\"" <> opts.strokeColor <> "\" " <>
  "stroke-width=\"" <> num opts.strokeWidth <> "\""

-- Format a number for SVG
num :: Number -> String
num n = toStringWith (fixed 2) n
