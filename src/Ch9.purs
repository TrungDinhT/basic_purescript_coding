module Ch9 where

import Ch7a (Maybe(..))
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


-- Commutative Typeclass
class Semigroup a <= Commutative a


-- Monoid Typeclass
class Semigroup a <= Monoid a where
    mempty :: a


-- Group Typeclass
class Monoid a <= Group a where
    ginverse :: a -> a


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


-- data type OrBool -> Boolean set with Logical Or operator
data OrBool = OFalse | OTrue
derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _
instance showOrBool :: Show OrBool where
    show = genericShow

-- Semigroup for OrBool
instance semigroupOrBool :: Semigroup OrBool where
    append OFalse OFalse = OFalse
    append _ _ = OTrue

-- Monoid for OrBool
instance monoidOrBool :: Monoid OrBool where
    mempty = OFalse


-- data type for Mod4
data Mod4 = Zero | One | Two | Three
derive instance eqMod4 :: Eq Mod4
derive instance genericMod4 :: Generic Mod4 _
instance showMod4 :: Show Mod4 where
    show = genericShow

-- Semigroup for Mod4
instance semigroupMod4 :: Semigroup Mod4 where
    append Zero x = x
    append x Zero = x

    append One One = Two
    append One Two = Three
    append One Three = Zero

    append Two One = Three
    append Two Two = Zero
    append Two Three = One

    append Three One = Zero
    append Three Two = One
    append Three Three = Two

-- Monoid for Mod4
instance monoidMod4 :: Monoid Mod4 where
    mempty = Zero

-- Group for Mod4
instance groupMod4 :: Group Mod4 where
    ginverse Zero = Zero
    ginverse One = Three
    ginverse Two = Two
    ginverse Three = One

-- Commutative for Mod4 (because the addition operator is actually commutative)
instance commutativeMod4 :: Commutative Mod4


-- Appending 2 Maybe -> return first result
newtype First a = First (Maybe a)
derive instance genericFirst :: Generic (First a) _
instance showFirst :: Show a => Show (First a) where
    show = genericShow

-- Semigroup for First
instance firstSemigroup :: Semigroup (First a) where
    append (First Nothing) last = last
    append first _ = first

-- Monoid for First
instance firstMonoid :: Monoid (First a) where
    mempty = First Nothing


-- Appending 2 Maybe -> return last result
newtype Last a = Last (Maybe a)
derive instance genericLast :: Generic (Last a) _
instance showLast :: Show a => Show (Last a) where
    show = genericShow

-- Semigroup for Last
instance lastSemigroup :: Semigroup (Last a) where
    append first (Last Nothing) = first
    append _ last = last

-- Monoid for Last
instance lastMonoid :: Monoid (Last a) where
    mempty = Last Nothing


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

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
    log "verify OrBool Semigroup (1 test)"
    log $ show $ (OFalse <> OTrue) <> OTrue == OFalse <> (OTrue <> OTrue)

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
    log "verify OrBool Monoid (2 tests)"
    log $ show $ OFalse <> mempty == mempty <> OFalse && OFalse <> mempty == OFalse
    log $ show $ OTrue <> mempty == mempty <> OTrue && OTrue <> mempty == OTrue

verifyMod4Semigroup :: Effect Unit
verifyMod4Semigroup = do
    log "verify Mod4 Semigroup (1 test)"
    log $ show $ (One <> Two) <> Three == One <> (Two <> Three)

verifyMod4Monoid :: Effect Unit
verifyMod4Monoid = do
    log "verify Mod4 Monoid (4 tests)"
    log $ show $ mempty <> Zero == Zero <> mempty && mempty <> Zero == Zero
    log $ show $ mempty <> One == One <> mempty && mempty <> One == One
    log $ show $ mempty <> Two == Two <> mempty && mempty <> Two == Two
    log $ show $ mempty <> Three == Three <> mempty && mempty <> Three == Three

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
    verifyOrBoolSemigroup
    verifyOrBoolMonoid
    verifyMod4Semigroup
    verifyMod4Monoid
    log $ show $ First Nothing <> First (Just 77)
    log $ show $ Last (Just 1) <> Last (Just 99)