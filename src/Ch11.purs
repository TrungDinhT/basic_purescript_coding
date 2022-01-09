module Ch11 where

import Data.List (List(..), foldl, (:))
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, Unit, show, discard, type (~>), ($), (>), negate)


-- reverse with foldl (because we want to pass the new list as the State to next recursive)
reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil 


-- max
max :: ∀ a. Ord a => a -> a -> a
max x y = if x > y then x else y


-- Test codes
test :: Effect Unit
test = do
    log $ show $ "Chap 11: Coding Folds"
    log $ show $ reverse (30 : 20 : 10 : Nil)
    log $ show $ max (-1) 99
    log $ show $ max "aa" "z"
