module Ch5 where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit)

test :: Effect Unit
test = do
  log "test"