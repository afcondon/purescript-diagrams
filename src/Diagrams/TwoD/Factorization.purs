-- | Factorization diagrams.
-- |
-- | This module implements the classic factorization diagram visualization,
-- | as popularized by Brent Yorgey and implemented in diagrams-contrib.
-- |
-- | Each number is visualized by recursively arranging dots according to its
-- | prime factorization. Primes create n-fold symmetry with colored polygons.
module Diagrams.TwoD.Factorization
  ( -- * Factorization
    factors
  , primeFactors

  -- * Color palette
  , defaultColors
  , primeColor

  -- * Diagram construction
  , factorDiagram
  , factorDiagram'
  , primeLayout

  -- * Grid display
  , fdGrid
  , fdGridList
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, cos, sin)

import Linear.V2 (V2(..))

import Diagrams.TwoD.Diagram (Diagram2D, fromShape, scaleD, rotateD, translateD, fillColor, strokeColor, strokeWidth, width, height)
import Diagrams.TwoD.Shapes (circle, regPoly, rect)
import Diagrams.TwoD.Combinators (ensquare, grid, centerXY)

-- | Prime factorization via trial division.
-- | Returns factors in ascending order with repetition.
factors :: Int -> Array Int
factors n = go n 2
  where
  go :: Int -> Int -> Array Int
  go m d
    | m < 2 = []
    | d * d > m = [m]  -- m is prime
    | m `mod` d == 0 = Array.cons d (go (m / d) d)
    | otherwise = go m (d + 1)

-- | Get unique prime factors (without repetition).
primeFactors :: Int -> Array Int
primeFactors = Array.nub <<< factors

-- | Default color palette for prime factors.
-- | Index 0 is for the base dot, then primes 2,3,5,7,11,13...
-- | Colors chosen for visual distinctness.
defaultColors :: Array String
defaultColors =
  [ "#1a1a1a"  -- 0: base dot (dark gray/black)
  , "#e41a1c"  -- 1: factor 2 (red)
  , "#ff7f00"  -- 2: factor 3 (orange)
  , "#ffff33"  -- 3: factor 5 (yellow)
  , "#4daf4a"  -- 4: factor 7 (green)
  , "#377eb8"  -- 5: factor 11 (blue)
  , "#984ea3"  -- 6: factor 13 (purple)
  , "#a65628"  -- 7: factor 17 (brown)
  , "#f781bf"  -- 8: factor 19 (pink)
  , "#999999"  -- 9: factor 23+ (gray)
  ]

-- | Get the color for a prime factor.
-- | Maps small primes to specific colors. Returns Nothing for large primes (dashed outline).
primeColor :: Int -> Maybe String
primeColor p = case p of
  2 -> Just "#e41a1c"   -- red
  3 -> Just "#ff7f00"   -- orange
  5 -> Just "#ffff33"   -- yellow
  7 -> Just "#4daf4a"   -- green
  11 -> Just "#377eb8"  -- blue
  13 -> Just "#984ea3"  -- purple
  17 -> Just "#a65628"  -- brown
  19 -> Just "#f781bf"  -- pink
  _ -> Nothing          -- large primes get dashed outline only

-- | Base dot color (dark gray).
baseDotColor :: String
baseDotColor = fromMaybe "#1a1a1a" (Array.index defaultColors 0)

-- | Create a colored circle (the base element).
-- | Size 0.3 gives good proportions when nested in factorization layouts.
baseDot :: Diagram2D
baseDot = fillColor baseDotColor $ strokeWidth 0.0 $ fromShape (circle 0.3)

-- | Arrange p copies of a diagram around a small colored p-gon center.
-- | The polygon is a small indicator in the middle, with items arranged around it.
-- | Special case: p=2 just places items side by side (no polygon).
primeLayout :: Int -> Diagram2D -> Diagram2D
primeLayout p d
  | p < 2 = d
  | p == 2 =
      -- Special case for 2: alternate direction to create balanced shapes
      -- Square or wide → stack vertically (elongate height)
      -- Tall → arrange horizontally (elongate width)
      -- This makes: 2 = vertical pair, 4 = 2×2 = square
      let w = width d
          h = height d
          subSize = max w h
          gap = subSize * 0.1  -- Small gap between the two copies
          offset = (subSize + gap) / 2.0
      in if h > w
         then -- Taller than wide: arrange horizontally to balance
           let left = translateD (V2 (negate offset) 0.0) d
               right = translateD (V2 offset 0.0) d
           in left <> right
         else -- Square or wider than tall: arrange vertically
           let top = translateD (V2 0.0 offset) d
               bottom = translateD (V2 0.0 (negate offset)) d
           in top <> bottom
  | otherwise =
      -- General case: small colored p-gon in center, items arranged around it
      let subSize = max (width d) (height d)
          -- Ring radius where items are placed (items should nearly touch)
          -- chord = 2 * R * sin(pi/p) = subSize, so R = subSize / (2 * sin(pi/p))
          ringRadius = subSize / (2.0 * sin (pi / toNumber p))
          -- Place items on the ring, surrounding the center
          copies = Array.range 0 (p - 1) # map \i ->
            let theta = 2.0 * pi * toNumber i / toNumber p - pi / 2.0
                dx = ringRadius * cos theta
                dy = ringRadius * sin theta
            in translateD (V2 dx dy) d
          combined = foldl (<>) mempty copies
          -- Small central polygon - the "nucleus" indicating the prime factor
          centerRadius = ringRadius * 0.35
          center = case primeColor p of
            Just c -> fillColor c $ strokeWidth 0.0 $ fromShape (regPoly p centerRadius)
            Nothing -> strokeColor "black" $ strokeWidth 0.02 $ fillColor "none" $ fromShape (regPoly p centerRadius)
      in center <> combined

-- | Create a factorization diagram for a positive integer.
-- | Uses default settings.
factorDiagram :: Int -> Diagram2D
factorDiagram = factorDiagram' baseDot

-- | Create a factorization diagram with a custom base element.
factorDiagram' :: Diagram2D -> Int -> Diagram2D
factorDiagram' base n
  | n < 1 = mempty
  | n == 1 = base
  | otherwise =
      let fs = factors n
      in foldl (\d p -> ensquare 0.1 (primeLayout p d)) base fs

-- | Create a grid of factorization diagrams from 1 to n×n.
fdGrid :: Int -> Array (Array Diagram2D)
fdGrid n = Array.range 1 n # map \row ->
  Array.range 1 n # map \col ->
    let num = (row - 1) * n + col
    in factorDiagram num

-- | Create a diagram showing a grid of factorization diagrams 1 to n×n.
fdGridList :: Int -> Diagram2D
fdGridList n = grid (fdGrid n)
