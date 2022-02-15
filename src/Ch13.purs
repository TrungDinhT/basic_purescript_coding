module Ch13 where

import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)
import Prelude (discard, show, ($), (/))


-- Functor Typeclass
class Functor f where
    map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>


-- Maybe data type
data Maybe a = Nothing | Just a
derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
    show = genericShow


-- Functor for Maybe
instance maybeFunctor :: Functor Maybe where
    map _ Nothing = Nothing
    map f (Just x) = Just $ f x 


-- Test codes
test :: Effect Unit
test = do
    log $ show $ "Chap 13: Coding Functors"
    log $ show $ (_ / 2) <$> Just 10
    log $ show $ (_ / 2) <$> Nothing
