module Ch13 where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)
import Prelude (discard, identity, show, ($), (/), (*), (<>), (==), (<<<))


-- Functor Typeclass
class Functor f where
    map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>



-- Maybe data type
data Maybe a = Nothing | Just a
derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
    show = genericShow


-- Either data type
data Either a b = Left a | Right b
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
    show = genericShow


-- Tuple data type
data Tuple a b = Tuple a b
derive instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)
derive instance genericTuple :: Generic (Tuple a b) _
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
    show = genericShow


-- Threeple data type
data Threeple a b c = Threeple a b c
derive instance genericThreeple :: Generic (Threeple a b c) _
instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
    show = genericShow



-- Functor for Maybe
instance maybeFunctor :: Functor Maybe where
    map _ Nothing = Nothing
    map f (Just x) = Just $ f x 


-- Functor for Either
instance eitherFunctor :: Functor (Either a) where
    map _ (Left err) = Left err
    map f (Right x) = Right $ f x


-- Functor for Tuple
instance tupleFunctor :: Functor (Tuple a) where
    map f (Tuple x y) = Tuple x $ f y


-- Functor for Threeple
instance threepleFunctor :: Functor (Threeple a b) where
    map f (Threeple x y z) = Threeple x y $ f z



-- Test codes
divideBy2 :: Int -> Int
divideBy2 = (_ / 2)

multiplyBy2 :: Int -> Int
multiplyBy2 = (_ * 2)

verifyMaybeIdentity :: Effect Unit
verifyMaybeIdentity = do
    log $ show $ "Maybe Identity for Nothing: " <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
    log $ show $ "Maybe Identity for Just: " <> show ((identity <$> Just 10) == Just 10)

verifyMaybeComposition :: Effect Unit
verifyMaybeComposition = do
    log $ show $ "Maybe composition for Nothing: " 
        <> show (map (divideBy2 <<< multiplyBy2) Nothing == (Nothing :: Maybe Int))
    log $ show $ "Maybe composition for Just: " 
        <> show (map (divideBy2 <<< multiplyBy2) (Just 10) == (map divideBy2 <<< map multiplyBy2) (Just 10))


test :: Effect Unit
test = do
    log $ show $ "Chap 13: Coding Functors"
    log $ show $ divideBy2 <$> Just 10
    log $ show $ divideBy2 <$> Nothing
    log $ show $ divideBy2 <$> (Right 10 :: Either Unit _)
    log $ show $ divideBy2 <$> Left "error reason"
    log $ show $ divideBy2 <$> Tuple 10 20
    log $ show $ divideBy2 <$> Threeple 10 20 40
    verifyMaybeIdentity
    verifyMaybeComposition
    log $ show $ "Tuple identity: " <> show ((identity <$> Tuple 10 20) == Tuple 10 20)
    log $ show $ "Tuple composition: " 
        <> show (map (divideBy2 <<< multiplyBy2) (Tuple 10 20) == (map divideBy2 <<< map multiplyBy2) (Tuple 10 20))
