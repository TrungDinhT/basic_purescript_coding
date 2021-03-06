module Ch7a where

import Prelude (Unit, show, discard, ($), (==), (<), (>), (<=), (>=))

import Data.Eq (class Eq)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import Effect (Effect)
import Effect.Console (log)

----------------------------------------------------------------------------

data Maybe a = Nothing | Just a

{-
    Write method for type classe Maybe
        - Eq for Maybe
        - Ord for Maybe
        - Function greaterThanOrEq and its companion operator >= (Commented, since it's only for learning purpose)
        - Show for Maybe
-}

{-
-- From-scratch version
instance eqMaybe :: Eq a => Eq (Maybe a) where
    eq Nothing Nothing = true
    eq (Just x) (Just y) = x == y
    eq _ _ = false
-}
-- Derived version
derive instance eqMaybe :: Eq a => Eq (Maybe a)

{-
-- From-scratch version
instance ordMaybe :: Ord a => Ord (Maybe a) where
    compare Nothing Nothing = EQ
    compare (Just x) (Just y) = compare x y 
    compare Nothing _ = LT
    compare _ Nothing = GT
-}
-- Derived version
derive instance ordMaybe :: Ord a => Ord (Maybe a)

{-
greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq x y = case compare x y of
    LT -> false
    _ -> true

-- infixl because the >= operator reads from left to right
infixl 4 greaterThanOrEq as >=
-}

{-
-- From-scratch version
instance showMaybe :: Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x) = "(Just " <> show x <> ")"
-}
derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
    show = genericShow

----------------------------------------------------------------------------

data Either a b = Left a | Right b

{-
    Write method for type classe Either
        - Eq for Maybe
        - Ord for Maybe
        - Show for Maybe
-}

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
    show = genericShow

{-
  Test function
-}

test :: Effect Unit
test = do
    log $ show $ "Chap 7a - Coding Typeclasses"
    log $ show $ Just 5 == Just 5
    log $ show $ Just 5 == Just 2
    log $ show $ Just 5 == Nothing
    log $ show $ Nothing == Just 5
    log $ show $ Nothing == (Nothing :: Maybe Unit)
    log "------------------"
    log $ show $ Just 1 < Just 5
    log $ show $ Just 5 <= Just 5
    log $ show $ Just 5 > Just 10
    log $ show $ Just 10 >= Just 10
    log $ show $ Just 99 > Nothing
    log $ show $ Just 99 < Nothing
    log "------------------"
    log $ show $ Just "abc"
    log $ show $ (Nothing :: Maybe Unit)
    log "------------------"
    log $ show $ (Left "left" :: Either _ Unit)
    log $ show $ (Right (Just 42) :: Either Unit _)

