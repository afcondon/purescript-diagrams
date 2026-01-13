-- | Demo: Diagrams + PSD3 Integration Concepts
-- |
-- | This demo illustrates how purescript-diagrams and PSD3 could work together:
-- | 1. Use diagrams for declarative shape construction
-- | 2. Use force simulation concepts for positioning
-- | 3. Render to SVG using diagrams' backend
module Demo.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Array ((..))
import Data.Foldable (fold)
import Data.Number (pi, cos, sin)

import Linear.V2 (V2(..))

import Diagrams.TwoD (circle, square, fromShape, beside, above, unitX, translateD)
import Diagrams.TwoD.Diagram (Diagram2D, width)
import Diagrams.Backend.SVG (renderSVGWith, defaultSVGOptions)

-- | A simple "node" with position (as if from a force simulation)
type Node =
  { id :: Int
  , x :: Number
  , y :: Number
  }

-- | Generate nodes in a circular layout (simulating force simulation output)
generateNodes :: Int -> Number -> Array Node
generateNodes n radius = (\i ->
  let angle = 2.0 * pi * (toNumber i) / (toNumber n)
  in { id: i
     , x: radius * cos angle
     , y: radius * sin angle
     }) <$> (0 .. (n - 1))
  where
  toNumber = \i -> if i == 0 then 0.0 else toNumber (i - 1) + 1.0

-- | Create a node shape using diagrams
nodeShape :: Number -> Diagram2D
nodeShape size = fromShape (circle size)

-- | Position a diagram at given coordinates (like force simulation would provide)
positionNode :: Node -> Diagram2D -> Diagram2D
positionNode node diagram = translateD (V2 node.x node.y) diagram

-- | Render a network graph with nodes positioned by "simulation"
renderNetwork :: Array Node -> String
renderNetwork nodes =
  let
    -- Create a diagram for each node at its position
    nodeDiagrams = nodes <#> \node ->
      positionNode node (nodeShape 10.0)

    -- Combine all nodes (overlay)
    combined = fold nodeDiagrams

    -- Render with custom options
    opts = defaultSVGOptions
      { width = 500.0
      , height = 500.0
      , strokeColor = "steelblue"
      , fillColor = "lightsteelblue"
      , fillOpacity = 0.8
      }
  in
    renderSVGWith opts combined

-- | Demo: Basic composition
demoComposition :: String
demoComposition =
  let
    c = fromShape (circle 1.0)
    s = fromShape (square 1.5)

    -- Compose horizontally then vertically
    row1 = beside unitX c s
    row2 = beside unitX s c
    grid = above row1 row2
  in
    renderSVGWith
      (defaultSVGOptions { strokeColor = "darkgreen", fillColor = "lightgreen", fillOpacity = 0.6 })
      grid

-- | Demo: Using envelopes for automatic spacing
demoEnvelopes :: Effect Unit
demoEnvelopes = do
  log "=== Envelope-based composition ==="
  let
    small = fromShape (circle 0.5)
    large = fromShape (circle 1.5)

    -- beside automatically uses envelopes to compute touching distance
    composed = beside unitX small large

  log $ "Small circle width: " <> show (width small)
  log $ "Large circle width: " <> show (width large)
  log $ "Composed width: " <> show (width composed)
  log "Notice: composed width = small + large (envelopes touching)"

main :: Effect Unit
main = do
  log "=== Diagrams + PSD3 Integration Demo ===\n"

  -- Demo 1: Basic composition
  log "1. Basic diagram composition:"
  log demoComposition
  log ""

  -- Demo 2: Envelope-based layout
  demoEnvelopes
  log ""

  -- Demo 3: Network with positioned nodes
  log "3. Network graph (nodes at circular positions):"
  let nodes = generateNodes 6 100.0
  log $ renderNetwork nodes

  log "\n=== Demo complete ==="
