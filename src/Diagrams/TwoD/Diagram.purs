-- | Two-dimensional diagrams with composition.
-- |
-- | A Diagram is a tree of shapes with transformations, supporting
-- | operations like overlay (atop), horizontal/vertical composition,
-- | and alignment.
module Diagrams.TwoD.Diagram
  ( -- * Diagram type
    Diagram2D(..)

  -- * Constructors
  , fromShape
  , empty

  -- * Composition
  , atop
  , beside
  , above
  , below

  -- * Transformation
  , translateD
  , scaleD
  , rotateD

  -- * Querying
  , envelope
  , width
  , height
  ) where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))

import Linear.V2 (V2(..))
import Linear.Affine (Point(..))
import Linear.Metric (norm)
import Linear.Vector ((*^), (^+^), negated)

import Diagrams.Core.Envelope (class Enveloped, Envelope, getEnvelope, mkEnvelope, appEnvelope, diameter)
import Diagrams.Core.Trace (class Traced, Trace, getTrace, mkTrace, appTrace, mkSortedList, getSortedList)
import Diagrams.Core.Transform (class Transformable, Transformation, transform, translation, scaling)
import Diagrams.Core.HasOrigin (class HasOrigin, moveOriginTo)
import Diagrams.TwoD.Types (P2, unitX, unitY)
import Diagrams.TwoD.Shapes (Shape2D)
import Diagrams.TwoD.Transform (rotation, T2)

-- | A 2D diagram is a tree of primitives with transformations.
data Diagram2D
  = Empty                              -- ^ Empty diagram
  | Prim Shape2D                       -- ^ A primitive shape
  | Transformed (T2 Number) Diagram2D  -- ^ A transformed diagram
  | Compose (List Diagram2D)           -- ^ Multiple diagrams overlaid

instance Show Diagram2D where
  show Empty = "Empty"
  show (Prim s) = "(Prim " <> show s <> ")"
  show (Transformed _ d) = "(Transformed ... " <> show d <> ")"
  show (Compose ds) = "(Compose " <> show ds <> ")"

-- | Semigroup instance: overlay diagrams (later diagrams on top)
instance Semigroup Diagram2D where
  append Empty d = d
  append d Empty = d
  append (Compose ds1) (Compose ds2) = Compose (ds1 <> ds2)
  append (Compose ds) d = Compose (ds <> List.singleton d)
  append d (Compose ds) = Compose (List.Cons d ds)
  append d1 d2 = Compose (List.Cons d1 (List.singleton d2))

instance Monoid Diagram2D where
  mempty = Empty

-- | Create a diagram from a shape.
fromShape :: Shape2D -> Diagram2D
fromShape = Prim

-- | The empty diagram.
empty :: Diagram2D
empty = Empty

-- | Place the first diagram atop the second (same as Semigroup <>).
atop :: Diagram2D -> Diagram2D -> Diagram2D
atop = append

-- Envelope instance: combine envelopes of all sub-diagrams
instance Enveloped V2 Number Diagram2D where
  getEnvelope Empty = mempty
  getEnvelope (Prim s) = getEnvelope s
  getEnvelope (Transformed t d) = transform t (getEnvelope d)
  getEnvelope (Compose ds) = go ds mempty
    where
    go :: List Diagram2D -> Envelope V2 Number -> Envelope V2 Number
    go List.Nil acc = acc
    go (List.Cons d rest) acc = go rest (append acc (getEnvelope d))

-- Trace instance: combine traces of all sub-diagrams
instance Traced V2 Number Diagram2D where
  getTrace Empty = mempty
  getTrace (Prim s) = getTrace s
  getTrace (Transformed t d) = transform t (getTrace d)
  getTrace (Compose ds) = go ds mempty
    where
    go :: List Diagram2D -> Trace V2 Number -> Trace V2 Number
    go List.Nil acc = acc
    go (List.Cons d rest) acc = go rest (append acc (getTrace d))

-- HasOrigin instance
instance HasOrigin V2 Number Diagram2D where
  moveOriginTo p Empty = Empty
  moveOriginTo p (Prim s) = Transformed (translation (negated (unP p))) (Prim s)
    where unP (P v) = v
  moveOriginTo p (Transformed t d) = Transformed (translation (negated (unP p)) <> t) d
    where unP (P v) = v
  moveOriginTo p (Compose ds) = Compose (map (moveOriginTo p) ds)

-- Transformable instance
instance Transformable V2 Number Diagram2D where
  transform _ Empty = Empty
  transform t (Prim s) = Transformed t (Prim s)
  transform t1 (Transformed t2 d) = Transformed (t1 <> t2) d
  transform t (Compose ds) = Compose (map (transform t) ds)

-- | Translate a diagram by a vector.
translateD :: V2 Number -> Diagram2D -> Diagram2D
translateD v = transform (translation v)

-- | Scale a diagram uniformly.
scaleD :: Number -> Diagram2D -> Diagram2D
scaleD s = transform (scaling s)

-- | Rotate a diagram by an angle (in radians).
rotateD :: Number -> Diagram2D -> Diagram2D
rotateD theta = transform (rotation theta)

-- | Get the envelope of a diagram in a given direction.
envelope :: V2 Number -> Diagram2D -> Maybe Number
envelope v d = appEnvelope (getEnvelope d) <*> pure v

-- | Get the width of a diagram.
width :: Diagram2D -> Number
width d = diameter unitX d

-- | Get the height of a diagram.
height :: Diagram2D -> Number
height d = diameter unitY d

-- | Place two diagrams beside each other in the given direction.
-- | The diagrams are translated so they are adjacent (touching envelopes).
beside :: V2 Number -> Diagram2D -> Diagram2D -> Diagram2D
beside v d1 d2 =
  case envelope v d1, envelope (negated v) d2 of
    Just e1, Just e2 ->
      let n = norm v
          v' = map (_ / n) v
      in d1 <> translateD ((e1 + e2) *^ v') d2
    _, _ -> d1 <> d2

-- | Place the second diagram above the first.
above :: Diagram2D -> Diagram2D -> Diagram2D
above = beside unitY

-- | Place the second diagram below the first.
below :: Diagram2D -> Diagram2D -> Diagram2D
below = beside (negated unitY)
