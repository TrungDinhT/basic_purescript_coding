module Ch11 where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, discard, type (~>), ($))


-- Test codes
test :: Effect Unit
test = do
    log $ show $ "Chap 11: Coding Folds"
