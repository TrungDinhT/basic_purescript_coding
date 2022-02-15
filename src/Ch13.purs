module Ch13 where

import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)
import Prelude (show, ($))

-- Functor Typeclass
class Functor f where
    map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>


-- Test codes
test :: Effect Unit
test = do
    log $ show $ "Chap 13: Coding Functors"
