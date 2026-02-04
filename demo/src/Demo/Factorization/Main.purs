module Demo.Factorization.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Diagrams.Backend.SVG (renderSVGWith, defaultSVGOptions)
import Diagrams.TwoD.Factorization (fdGridList)

foreign import setInnerHTML :: String -> String -> Effect Unit

main :: Effect Unit
main = do
  log "Generating factorization diagrams..."
  let diagram = fdGridList 10  -- 10x10 grid (1-100)
      opts = defaultSVGOptions { width = 800.0, height = 800.0 }
      svg = renderSVGWith opts diagram
  setInnerHTML "diagram-container" svg
  log "Done!"
