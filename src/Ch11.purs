module Ch11 where

import Data.List (List(..), foldl, (:))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, discard, type (~>), ($))


-- reverse with foldl (because we want to pass the new list as the State to next recursive)
reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil 


-- Test codes
test :: Effect Unit
test = do
    log $ show $ "Chap 11: Coding Folds"
    log $ show $ reverse (30 : 20 : 10 : Nil)
