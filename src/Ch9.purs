module Ch9 where

import Prelude (Unit, ($), show)

import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
    log $ show $ "Chap 9 - Coding Abstract Algebra"