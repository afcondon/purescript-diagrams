-- | Queries: functions from points in a vector space to some monoid.
-- |
-- | The idea for annotating diagrams with monoidal queries came from
-- | the graphics-drawingcombinators Haskell package. A common use case
-- | is using `Any` as the monoid to detect whether a point is inside
-- | a diagram.
module Diagrams.Core.Query
  ( Query(..)
  , runQuery
  , point
  ) where

import Prelude

import Linear.Affine (Point(..), (.+^))
import Linear.Vector (class Additive)

import Diagrams.Core.HasOrigin (class HasOrigin)
import Diagrams.Core.Transform (class Transformable, Transformation, inv, papply)

-- | A query is a function that maps points in a vector space to
-- | values in some monoid. Queries naturally form a monoid, with
-- | two queries being combined pointwise.
newtype Query v n m = Query (Point v n -> m)

-- | Run a query at a given point.
runQuery :: forall v n m. Query v n m -> Point v n -> m
runQuery (Query f) = f

-- | Create a query that returns the given value for all points.
point :: forall v n m. m -> Query v n m
point m = Query (const m)

instance Functor (Query v n) where
  map f (Query q) = Query (f <<< q)

instance Apply (Query v n) where
  apply (Query qf) (Query qa) = Query \p -> qf p (qa p)

instance Applicative (Query v n) where
  pure m = Query (const m)

instance Bind (Query v n) where
  bind (Query q) f = Query \p -> runQuery (f (q p)) p

instance Monad (Query v n)

instance Semigroup m => Semigroup (Query v n m) where
  append (Query q1) (Query q2) = Query \p -> q1 p <> q2 p

instance Monoid m => Monoid (Query v n m) where
  mempty = Query (const mempty)

instance (Additive v, Semiring n) => HasOrigin v n (Query v n m) where
  moveOriginTo (P u) (Query q) = Query \pt -> q (pt .+^ u)

instance (Additive v, Ring n) => Transformable v n (Query v n m) where
  transform t (Query q) = Query \p -> q (papply (inv t) p)
