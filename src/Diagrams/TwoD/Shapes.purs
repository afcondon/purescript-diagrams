-- | Two-dimensional shapes with envelopes and traces.
-- |
-- | This module provides basic 2D shapes (circles, rectangles, etc.) as
-- | concrete data types with Enveloped and Traced instances.
module Diagrams.TwoD.Shapes
  ( -- * Shape type
    Shape2D(..)

  -- * Circle
  , circle
  , unitCircle

  -- * Rectangle
  , rect
  , square
  , unitSquare

  -- * Line segments
  , hrule
  , vrule

  -- * Polygon
  , polygon
  , regPoly
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, maximumBy)
import Data.Int (toNumber)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Number (sqrt, abs, pi, cos, sin)
import Data.Ord (comparing)

import Linear.V2 (V2(..))
import Linear.Affine (Point(..), origin, (.-.), (.+^))
import Linear.Vector (negated, (*^))
import Linear.Metric (dot, norm)

import Diagrams.Core.Envelope (class Enveloped, Envelope, mkEnvelope)
import Diagrams.Core.Trace (class Traced, Trace, mkTrace, mkSortedList)
import Diagrams.Core.Transform (class Transformable, Transformation, papply)
import Diagrams.Core.HasOrigin (class HasOrigin)
import Diagrams.TwoD.Types (P2, mkP2)

-- | A 2D shape centered at the origin.
data Shape2D
  = Circle Number              -- ^ Circle with given radius
  | Rectangle Number Number    -- ^ Rectangle with given width and height
  | LineSegment (V2 Number)    -- ^ Line segment from origin in given direction
  | Polygon (Array (V2 Number)) -- ^ Polygon with vertices relative to centroid

derive instance Eq Shape2D

instance Show Shape2D where
  show (Circle r) = "(Circle " <> show r <> ")"
  show (Rectangle w h) = "(Rectangle " <> show w <> " " <> show h <> ")"
  show (LineSegment v) = "(LineSegment " <> show v <> ")"
  show (Polygon vs) = "(Polygon " <> show vs <> ")"

-- Circle envelope: for a unit circle centered at origin,
-- the envelope in direction v is simply 1 (the radius)
circleEnvelope :: Number -> Envelope V2 Number
circleEnvelope radius = mkEnvelope \v ->
  radius / norm v

-- Rectangle envelope: the envelope is the support function of the rectangle
rectEnvelope :: Number -> Number -> Envelope V2 Number
rectEnvelope w h = mkEnvelope \(V2 vx vy) ->
  let halfW = w / 2.0
      halfH = h / 2.0
      -- The envelope of a rectangle is max of projections onto corners
      -- For an axis-aligned rectangle, this simplifies to:
      n = norm (V2 vx vy)
  in (abs vx * halfW + abs vy * halfH) / n

-- Line segment envelope
lineEnvelope :: V2 Number -> Envelope V2 Number
lineEnvelope seg@(V2 sx sy) = mkEnvelope \v ->
  let n = norm v
      -- Project both endpoints onto direction v
      -- Origin projects to 0
      -- Endpoint projects to (seg · v) / |v|²
      proj = dot seg v / (dot v v)
  in max 0.0 proj

-- Polygon envelope: max projection of vertices onto the direction
polygonEnvelope :: Array (V2 Number) -> Envelope V2 Number
polygonEnvelope vertices = mkEnvelope \v ->
  let n = norm v
      projections = map (\vtx -> dot vtx v / n) vertices
  in foldl max 0.0 projections

instance Enveloped V2 Number Shape2D where
  getEnvelope (Circle r) = circleEnvelope r
  getEnvelope (Rectangle w h) = rectEnvelope w h
  getEnvelope (LineSegment v) = lineEnvelope v
  getEnvelope (Polygon vs) = polygonEnvelope vs

-- Circle trace: intersections of a ray with a circle
circleTrace :: Number -> Trace V2 Number
circleTrace radius = mkTrace \(P (V2 px py)) (V2 vx vy) ->
  -- Ray: P + t*V
  -- Circle: x² + y² = r²
  -- Substituting: (px + t*vx)² + (py + t*vy)² = r²
  -- Expanding: t²(vx² + vy²) + 2t(px*vx + py*vy) + (px² + py² - r²) = 0
  let a = vx * vx + vy * vy
      b = 2.0 * (px * vx + py * vy)
      c = px * px + py * py - radius * radius
      discriminant = b * b - 4.0 * a * c
  in if discriminant < 0.0
     then mkSortedList List.Nil
     else
       let sqrtD = sqrt discriminant
           t1 = (negate b - sqrtD) / (2.0 * a)
           t2 = (negate b + sqrtD) / (2.0 * a)
       in mkSortedList (List.Cons t1 (List.Cons t2 List.Nil))

-- Rectangle trace: intersections with axis-aligned rectangle
rectTrace :: Number -> Number -> Trace V2 Number
rectTrace w h = mkTrace \(P (V2 px py)) (V2 vx vy) ->
  let halfW = w / 2.0
      halfH = h / 2.0
      -- Calculate intersections with each edge
      intersections = List.catMaybes $ List.fromFoldable
        [ edgeIntersect px vx (negate halfW)  -- left edge
        , edgeIntersect px vx halfW           -- right edge
        , edgeIntersect py vy (negate halfH)  -- bottom edge
        , edgeIntersect py vy halfH           -- top edge
        ]
      -- Filter to only keep intersections actually on the rectangle
      valid = List.filter (isOnRect halfW halfH px py vx vy) intersections
  in mkSortedList valid
  where
  edgeIntersect :: Number -> Number -> Number -> Maybe Number
  edgeIntersect p v edge
    | abs v < 1.0e-10 = Nothing  -- Parallel to edge
    | otherwise = Just ((edge - p) / v)

  isOnRect :: Number -> Number -> Number -> Number -> Number -> Number -> Number -> Boolean
  isOnRect halfW halfH px py vx vy t =
    let x = px + t * vx
        y = py + t * vy
    in abs x <= halfW + 1.0e-10 && abs y <= halfH + 1.0e-10

-- Line segment trace (no boundary, so empty trace)
lineTrace :: V2 Number -> Trace V2 Number
lineTrace _ = mkTrace \_ _ -> mkSortedList List.Nil

-- Polygon trace: intersections of a ray with polygon edges
polygonTrace :: Array (V2 Number) -> Trace V2 Number
polygonTrace vertices = mkTrace \(P (V2 px py)) (V2 vx vy) ->
  let n = Array.length vertices
      -- Generate edges: each edge is (v_i, v_{i+1})
      -- For each edge, compute ray-segment intersection
      intersections = Array.catMaybes $ Array.mapWithIndex (\i v1 ->
        let j = (i + 1) `mod` n
            v2 = case Array.index vertices j of
              Just v -> v
              Nothing -> v1  -- shouldn't happen
        in raySegmentIntersect px py vx vy v1 v2
      ) vertices
  in mkSortedList (List.fromFoldable intersections)
  where
  -- Ray-segment intersection
  -- Ray: P + t * D
  -- Segment: A + u * (B - A) for u in [0, 1]
  raySegmentIntersect :: Number -> Number -> Number -> Number -> V2 Number -> V2 Number -> Maybe Number
  raySegmentIntersect px py dx dy (V2 ax ay) (V2 bx by) =
    let sx = bx - ax  -- segment direction x
        sy = by - ay  -- segment direction y
        denom = dx * sy - dy * sx
    in if abs denom < 1.0e-10
       then Nothing  -- parallel
       else
         let t = ((ax - px) * sy - (ay - py) * sx) / denom
             u = ((ax - px) * dy - (ay - py) * dx) / denom
         in if u >= 0.0 && u <= 1.0
            then Just t
            else Nothing

instance Traced V2 Number Shape2D where
  getTrace (Circle r) = circleTrace r
  getTrace (Rectangle w h) = rectTrace w h
  getTrace (LineSegment v) = lineTrace v
  getTrace (Polygon vs) = polygonTrace vs

-- HasOrigin instance (shapes are centered, so moving origin translates them)
-- Note: This is a simplified implementation - a full implementation would
-- track the origin offset within the shape
instance HasOrigin V2 Number Shape2D where
  moveOriginTo _ shape = shape  -- For now, shapes are always centered

-- Transformable instance
-- Note: This is simplified - proper transformation would need to track
-- the accumulated transformation
instance Transformable V2 Number Shape2D where
  transform t (Circle r) = Circle r  -- Scale would change radius
  transform t (Rectangle w h) = Rectangle w h
  transform t (LineSegment v) = LineSegment v
  transform t (Polygon vs) = Polygon vs

-- | A circle of the given radius, centered at the origin.
circle :: Number -> Shape2D
circle = Circle

-- | A circle of radius 1, centered at the origin.
unitCircle :: Shape2D
unitCircle = Circle 1.0

-- | A rectangle with the given width and height, centered at the origin.
rect :: Number -> Number -> Shape2D
rect = Rectangle

-- | A square with sides of the given length, centered at the origin.
square :: Number -> Shape2D
square s = Rectangle s s

-- | A unit square (side length 1), centered at the origin.
unitSquare :: Shape2D
unitSquare = Rectangle 1.0 1.0

-- | A horizontal line segment of the given length, centered at the origin.
hrule :: Number -> Shape2D
hrule len = LineSegment (V2 len 0.0)

-- | A vertical line segment of the given length, centered at the origin.
vrule :: Number -> Shape2D
vrule len = LineSegment (V2 0.0 len)

-- | A polygon with the given vertices (relative to centroid).
polygon :: Array (V2 Number) -> Shape2D
polygon = Polygon

-- | A regular polygon with n sides and circumradius r, centered at the origin.
-- | The first vertex is at the top (12 o'clock position).
regPoly :: Int -> Number -> Shape2D
regPoly n r = Polygon $ Array.range 0 (n - 1) # map \i ->
  let theta = 2.0 * pi * toNumber i / toNumber n - pi / 2.0
  in V2 (r * cos theta) (r * sin theta)
