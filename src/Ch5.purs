module Ch5 where

import Prelude (Unit, (+), show, discard)

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)

{-
  Write functions in Data.Function
    - flip (reverse the order of a function with 2 arguments)
    - const (return the first element <=> a const function)
    - apply and operator $ (apply a function => to chain the functions together)
    - applyFlipped and operator # (also to apply a function, but the argument comes before the function)
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
  Write functions in Data.List
    - singleton (create a list of one element)
    - null (check if a list is empty)
    - snoc (add an element to the end of the list)
    - length (get the length of a list)
-}

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

length :: ∀ a. List a -> Int
length Nil = 0
length (_ : xs) = 1 + length xs

{-
  Test function
-}

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  log $ flip const 1 2 # show
  flip const 1 2 # show # log
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length $ 1 : 2 : 3 : Nil