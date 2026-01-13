-- | Core diagrams modules.
-- |
-- | This module re-exports the core abstractions for building diagrams:
-- | - Transformations (translate, scale, etc.)
-- | - Envelopes (functional bounding regions)
-- | - Traces (ray-casting)
-- | - Queries (point-to-monoid functions)
module Diagrams.Core
  ( module Diagrams.Core.HasOrigin
  , module Diagrams.Core.Transform
  , module Diagrams.Core.Envelope
  , module Diagrams.Core.Trace
  , module Diagrams.Core.Query
  ) where

import Diagrams.Core.HasOrigin
  ( class HasOrigin
  , moveOriginTo
  , moveOriginBy
  , moveTo
  , place
  )

import Diagrams.Core.Transform
  ( InvLinear(..)
  , mkInvLinear
  , linv
  , lapp
  , Transformation(..)
  , inv
  , transp
  , transl
  , dropTransl
  , apply
  , papply
  , fromLinear
  , fromOrthogonal
  , fromSymmetric
  , class Transformable
  , transform
  , translation
  , translate
  , scaling
  , scale
  )

import Diagrams.Core.Envelope
  ( Envelope(..)
  , appEnvelope
  , onEnvelope
  , mkEnvelope
  , pointEnvelope
  , class Enveloped
  , getEnvelope
  , diameter
  , radius
  , extent
  , envelopeVMay
  , envelopeV
  , envelopePMay
  , envelopeP
  )

import Diagrams.Core.Trace
  ( SortedList
  , mkSortedList
  , getSortedList
  , onSortedList
  , Trace(..)
  , appTrace
  , mkTrace
  , class Traced
  , getTrace
  , traceV
  , traceP
  , maxTraceV
  , maxTraceP
  , rayTraceV
  , rayTraceP
  )

import Diagrams.Core.Query
  ( Query(..)
  , runQuery
  , point
  )
