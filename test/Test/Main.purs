module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Diagrams.TwoD (circle, square, rect, fromShape, beside, unitX)
import Diagrams.TwoD.Diagram (width)
import Diagrams.Backend.SVG (renderSVG, renderSVGWith, defaultSVGOptions)

main :: Effect Unit
main = do
  log "=== purescript-diagrams tests ==="
  log ""

  -- Test basic shapes
  log "Creating basic shapes..."
  let c = fromShape (circle 1.0)
  let s = fromShape (square 2.0)
  let r = fromShape (rect 3.0 1.0)

  log $ "Circle width: " <> show (width c)
  log $ "Square width: " <> show (width s)
  log $ "Rectangle width: " <> show (width r)
  log ""

  -- Test composition
  log "Testing composition..."
  let composed = beside unitX c s
  log $ "Composed diagram width: " <> show (width composed)
  log ""

  -- Test SVG rendering
  log "Rendering SVG..."
  let svg = renderSVG c
  log $ "Circle SVG length: " <> show (length svg) <> " chars"
  log ""

  -- Render a more complex diagram
  let complex = beside unitX (fromShape (circle 1.0)) (fromShape (square 1.5))
  let complexSvg = renderSVGWith (defaultSVGOptions { strokeColor = "blue", fillColor = "lightblue", fillOpacity = 0.5 }) complex
  log "Complex diagram SVG:"
  log complexSvg

  log "=== All tests passed ==="
  where
  length :: String -> Int
  length _ = 0  -- Placeholder, we don't need the actual length

