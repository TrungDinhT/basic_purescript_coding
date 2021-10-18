module Ch5 where

import Prelude (Unit, show, discard)

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)

{-
  Write functions in Data.Function
    - flip
    - const
    - apply and operator $
    - applyFlipped and operator #
-}

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f = f

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

-- infixl is because it's applyFlipped, we want everything to be regrouped on the left
-- precedence = 1 is because we want it to be executed at last but just before the $ op
infixl 1 applyFlipped as #

{-
  Test function
-}

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  log $ flip const 1 2 # show
  flip const 1 2 # show # log