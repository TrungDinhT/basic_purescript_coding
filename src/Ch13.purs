module Ch13 where

import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)
import Prelude (show, ($))

-- Test codes
test :: Effect Unit
test = do
    log $ show $ "Chap 13: Coding Functors"
