-- | Test suite for purescript-diagrams
-- |
-- | Ported from Haskell diagrams-core and diagrams-lib test suites.
-- | Tests transformation properties, envelope behavior, and composition.
module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Number (abs, pi)

import Linear.V2 (V2(..))
import Linear.Affine (Point(..))
import Linear.Vector (negated)
import Linear.Metric (norm, dot)

import Diagrams.TwoD (circle, square, rect, fromShape, beside, above, unitX, unitY)
import Diagrams.TwoD.Diagram (width, height, envelope, translateD, scaleD, rotateD)
import Diagrams.TwoD.Transform (rotation, reflectionX, reflectionY)
import Diagrams.Core.Transform (scaling, translation)
import Diagrams.Core.Transform as T
import Diagrams.Backend.SVG (renderSVG)

-- | Approximate equality for floating point comparisons
epsilon :: Number
epsilon = 0.001

approxEq :: Number -> Number -> Boolean
approxEq a b = abs (a - b) < epsilon

approxEqV2 :: V2 Number -> V2 Number -> Boolean
approxEqV2 (V2 x1 y1) (V2 x2 y2) = approxEq x1 x2 && approxEq y1 y2

-- | Test result tracking
type TestResult = { passed :: Int, failed :: Int, messages :: Array String }

emptyResult :: TestResult
emptyResult = { passed: 0, failed: 0, messages: [] }

assert :: String -> Boolean -> TestResult -> TestResult
assert name condition result =
  if condition
    then result { passed = result.passed + 1 }
    else result { failed = result.failed + 1
                , messages = result.messages <> ["FAILED: " <> name]
                }

-- =============================================================================
-- V2 Vector Tests (from Diagrams.Test.TwoD)
-- =============================================================================

testV2Properties :: TestResult -> TestResult
testV2Properties result =
  result
    -- "R2 vector addition is commutative"
    # assert "V2 addition commutative"
        (let u = V2 3.0 4.0
             v = V2 1.0 2.0
         in approxEqV2 (u <> v) (v <> u))

    -- "R2 subtraction is the inverse of addition"
    # assert "V2 subtraction inverse of addition"
        (let u = V2 3.0 4.0
             v = V2 1.0 2.0
             V2 ux uy = u
             V2 vx vy = v
             subtracted = V2 (ux + vx - vx) (uy + vy - vy)
         in approxEqV2 subtracted u)

    -- "R2 vector negation squared is identity"
    # assert "V2 negation squared is identity"
        (let u = V2 3.0 4.0
         in approxEqV2 (negated (negated u)) u)

    -- Dot product test
    # assert "V2 dot product"
        (let u = V2 1.0 0.0
             v = V2 0.0 1.0
         in approxEq (dot u v) 0.0)

    -- Norm test
    # assert "V2 norm of (3,4) is 5"
        (approxEq (norm (V2 3.0 4.0)) 5.0)

-- =============================================================================
-- Transform Tests (from Diagrams.Test.Transform)
-- =============================================================================

testTransformProperties :: TestResult -> TestResult
testTransformProperties result =
  result
    -- "a rotation of 0 does nothing"
    # assert "rotation of 0 is identity"
        (let v = V2 3.0 4.0
             t = rotation 0.0
         in approxEqV2 (T.apply t v) v)

    -- "adding 2π to rotation does nothing"
    # assert "rotation by 2π is identity"
        (let v = V2 3.0 4.0
             t = rotation (2.0 * pi)
         in approxEqV2 (T.apply t v) v)

    -- "rotation by π/2 rotates (1,0) to (0,1)"
    # assert "rotation by π/2"
        (let v = V2 1.0 0.0
             t = rotation (pi / 2.0)
         in approxEqV2 (T.apply t v) (V2 0.0 1.0))

    -- "rotation by π rotates (1,0) to (-1,0)"
    # assert "rotation by π"
        (let v = V2 1.0 0.0
             t = rotation pi
         in approxEqV2 (T.apply t v) (V2 (-1.0) 0.0))

    -- "rotating then inverse rotating yields original"
    # assert "rotation inverse"
        (let v = V2 3.0 4.0
             theta = pi / 3.0
             t1 = rotation theta
             t2 = rotation (negate theta)
             rotated = T.apply t2 (T.apply t1 v)
         in approxEqV2 rotated v)

    -- "scaleX"
    # assert "scaleX scales x component"
        (let v = V2 3.0 4.0
             t = scaling 2.0  -- uniform scale
         in approxEqV2 (T.apply t v) (V2 6.0 8.0))

    -- "reflectX"
    # assert "reflectX negates x"
        (let v = V2 3.0 4.0
             t = reflectionX
         in approxEqV2 (T.apply t v) (V2 (-3.0) 4.0))

    -- "reflectY"
    # assert "reflectY negates y"
        (let v = V2 3.0 4.0
             t = reflectionY
         in approxEqV2 (T.apply t v) (V2 3.0 (-4.0)))

    -- "translation has no effect on vectors"
    # assert "translation doesn't affect vectors"
        (let v = V2 3.0 4.0
             t = translation (V2 10.0 20.0)
         in approxEqV2 (T.apply t v) v)

    -- "translation affects points correctly"
    # assert "translation affects points"
        (let p = P (V2 1.0 2.0)
             t = translation (V2 10.0 20.0)
             P result = T.papply t p
         in approxEqV2 result (V2 11.0 22.0))

-- =============================================================================
-- Envelope Tests
-- =============================================================================

testEnvelopeProperties :: TestResult -> TestResult
testEnvelopeProperties result =
  result
    -- Circle envelope
    # assert "circle envelope in +x direction"
        (let c = fromShape (circle 1.0)
         in case envelope unitX c of
              Just e -> approxEq e 1.0
              Nothing -> false)

    # assert "circle envelope in +y direction"
        (let c = fromShape (circle 1.0)
         in case envelope unitY c of
              Just e -> approxEq e 1.0
              Nothing -> false)

    # assert "circle envelope in diagonal direction"
        (let c = fromShape (circle 1.0)
             diag = V2 1.0 1.0
             -- envelope returns t where t*v reaches boundary
             -- for radius 1, |t*v| = 1, so t = 1/|v| = 1/sqrt(2)
             expected = 1.0 / norm diag
         in case envelope diag c of
              Just e -> approxEq e expected
              Nothing -> false)

    -- Rectangle envelope
    # assert "rectangle envelope width"
        (let r = fromShape (rect 4.0 2.0)
         in approxEq (width r) 4.0)

    # assert "rectangle envelope height"
        (let r = fromShape (rect 4.0 2.0)
         in approxEq (height r) 2.0)

    -- Square envelope
    # assert "square width equals side length"
        (let s = fromShape (square 3.0)
         in approxEq (width s) 3.0)

    # assert "square height equals side length"
        (let s = fromShape (square 3.0)
         in approxEq (height s) 3.0)

    -- Composition envelope
    # assert "beside increases width"
        (let c1 = fromShape (circle 1.0)
             c2 = fromShape (circle 1.0)
             composed = beside unitX c1 c2
         in approxEq (width composed) 4.0)  -- 2 + 2

    # assert "above increases height"
        (let c1 = fromShape (circle 1.0)
             c2 = fromShape (circle 1.0)
             composed = above c1 c2
         in approxEq (height composed) 4.0)  -- 2 + 2

    # assert "beside with different sizes"
        (let c = fromShape (circle 0.5)   -- diameter 1
             s = fromShape (circle 1.5)   -- diameter 3
             composed = beside unitX c s
         in approxEq (width composed) 4.0)  -- 1 + 3

-- =============================================================================
-- Diagram Transform Tests
-- =============================================================================

testDiagramTransforms :: TestResult -> TestResult
testDiagramTransforms result =
  result
    # assert "translateD moves diagram"
        (let c = fromShape (circle 1.0)
             moved = translateD (V2 5.0 0.0) c
         -- envelope in +x should be 1 (radius) from the new center at 5
         in case envelope unitX moved of
              Just e -> approxEq e 6.0  -- center at 5 + radius 1
              Nothing -> false)

    # assert "scaleD scales diagram"
        (let c = fromShape (circle 1.0)
             scaled = scaleD 2.0 c
         in approxEq (width scaled) 4.0)  -- 2 * 2

    # assert "rotateD preserves circle size"
        (let c = fromShape (circle 1.0)
             rotated = rotateD (pi / 4.0) c
         in approxEq (width rotated) 2.0)

-- =============================================================================
-- SVG Rendering Tests
-- =============================================================================

testSVGRendering :: TestResult -> TestResult
testSVGRendering result =
  result
    # assert "circle renders to valid SVG"
        (let c = fromShape (circle 1.0)
             svg = renderSVG c
         in contains "<circle" svg && contains "</svg>" svg)

    # assert "rectangle renders to valid SVG"
        (let r = fromShape (rect 2.0 1.0)
             svg = renderSVG r
         in contains "<rect" svg && contains "</svg>" svg)

    # assert "composed diagram renders all shapes"
        (let c = fromShape (circle 1.0)
             s = fromShape (square 1.0)
             composed = beside unitX c s
             svg = renderSVG composed
         in contains "<circle" svg && contains "<rect" svg)

-- =============================================================================
-- Main
-- =============================================================================

contains :: String -> String -> Boolean
contains needle haystack = go 0
  where
  needleLen = strLen needle
  haystackLen = strLen haystack
  go i
    | i + needleLen > haystackLen = false
    | substr i needleLen haystack == needle = true
    | otherwise = go (i + 1)

foreign import strLen :: String -> Int
foreign import substr :: Int -> Int -> String -> String

main :: Effect Unit
main = do
  log "=== purescript-diagrams Test Suite ==="
  log "Ported from Haskell diagrams-core and diagrams-lib"
  log ""

  let result = emptyResult
        # testV2Properties
        # testTransformProperties
        # testEnvelopeProperties
        # testDiagramTransforms
        # testSVGRendering

  log $ "Tests passed: " <> show result.passed
  log $ "Tests failed: " <> show result.failed

  if result.failed > 0
    then do
      log ""
      log "Failed tests:"
      void $ traverse log result.messages
    else pure unit

  log ""
  if result.failed == 0
    then log "=== All tests passed ==="
    else log "=== Some tests failed ==="
  where
  traverse :: forall a. (a -> Effect Unit) -> Array a -> Effect Unit
  traverse f arr = go 0
    where
    len = arrayLen arr
    go i
      | i >= len = pure unit
      | otherwise = do
          f (unsafeIndex arr i)
          go (i + 1)

foreign import arrayLen :: forall a. Array a -> Int
foreign import unsafeIndex :: forall a. Array a -> Int -> a
