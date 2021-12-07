module Ch7a where

import Prelude (Unit, show, discard, ($), (==), (<), (>), (<=), (<>))

import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show)

import Effect (Effect)
import Effect.Console (log)

data Maybe a = Nothing | Just a

{-
    Write method for type classes
        - Eq for Maybe
        - Ord for Maybe
        - Function greaterThanOrEq and its companion operator >=
        - Show for Maybe
-}

instance eqMaybe :: Eq a => Eq (Maybe a) where
    eq Nothing Nothing = true
    eq (Just x) (Just y) = x == y
    eq _ _ = false

instance ordMaybe :: Ord a => Ord (Maybe a) where
    compare Nothing Nothing = EQ
    compare (Just x) (Just y) = compare x y 
    compare Nothing _ = LT
    compare _ Nothing = GT

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq x y = case compare x y of
    LT -> false
    _ -> true

-- infixl because the >= operator reads from left to right
infixl 4 greaterThanOrEq as >=

instance showMaybe :: Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x) = "(Just " <> show x <> ")"

{-
  Test function
-}

test :: Effect Unit
test = do
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

