module Ch9 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, show, ($), discard, (==), (&&))


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


-- data type AndBool -> Boolean set with Logical And operator
data AndBool = AFalse | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _
instance showAndBool :: Show AndBool where
    show = genericShow

-- Semigroup for AndBool
instance semigroupAndBool :: Semigroup AndBool where
    append ATrue ATrue = ATrue
    append _ _ = AFalse


-- Monoid for AndBool
instance monoidAndBool :: Monoid AndBool where
    mempty = ATrue


-- Test codes
verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
    log "verify AndBool Semigroup (1 test)"
    log $ show $ (ATrue <> ATrue) <> AFalse == ATrue <> (ATrue <> AFalse)

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
    log "verify AndBool Monoid (2 tests)"
    log $ show $ ATrue <> mempty == ATrue && mempty <> ATrue == ATrue
    log $ show $ AFalse <> mempty == AFalse && mempty <> AFalse == AFalse

test :: Effect Unit
test = do
    log $ show $ "Chap 9 - Coding Abstract Algebra"
    log $ show $ ATrue <> ATrue
    log $ show $ ATrue <> AFalse
    log $ show $ AFalse <> AFalse
    log $ show $ mempty <> ATrue == ATrue
    log $ show $ mempty <> AFalse == AFalse
    verifyAndBoolSemigroup
    verifyAndBoolMonoid