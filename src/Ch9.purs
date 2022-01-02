module Ch9 where

import Prelude (Unit, ($), show)

import Effect (Effect)
import Effect.Console (log)
-- Semigroup Typeclass
class Semigroup a where
    append :: a -> a -> a

{-
-- Thought: append is infixr, because:
--  - if it's infix, when having multiple <>, we need to precise where to be executed first with (), 
--  which is not correct for Semigroup where the operator is associative
--  - if it's infixl, we have the tendence to think that it's executed from left to right, which is not clear
--  to show that it's associative??
--  => We use infixr ??
-}
infixr 5 append as <>


-- Monoid Typeclass
class Semigroup a <= Monoid a where
    mempty :: a


-- Semigroup for AndBool

test :: Effect Unit
test = do
    log $ show $ "Chap 9 - Coding Abstract Algebra"