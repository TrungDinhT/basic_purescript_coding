module Ch5 where

import Prelude (Unit, show, discard)

import Effect (Effect)
import Effect.Console (log)

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f = f

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  log $ flip const 1 2 # show
  flip const 1 2 # show # log