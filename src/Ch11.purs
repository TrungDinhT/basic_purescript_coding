module Ch11 where

import Data.List (List(..), (:), foldl)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Semigroup.Foldable (foldl1)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, Unit, show, discard, type (~>), ($), (>), negate)


-- reverse with foldl (because we want to pass the new list as the State to next recursive)
reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil 


-- max
max :: ∀ a. Ord a => a -> a -> a
max x y = if x > y then x else y


-- findMax
findMax :: ∀ a. Ord a => List a -> Maybe a
{-
-- Manual coding
findMax Nil = Nothing
findMax (head : tail) = Just $ go head tail where
    go mx Nil = mx
    go mx (x : xs) = go (max mx x) xs
-}
-- Using Fold
findMax Nil = Nothing
findMax (head : tail) = Just $ foldl max head tail 


-- findMaxNE
findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
{-
-- Using foldl
findMaxNE (NonEmptyList (NonEmpty head tail)) = foldl max head tail
-}
-- Using foldl1
findMaxNE (NonEmptyList l) = foldl1 max l


-- Test codes
test :: Effect Unit
test = do
    log $ show $ "Chap 11: Coding Folds"
    log $ show $ reverse (30 : 20 : 10 : Nil)
    log $ show $ max (-1) 99
    log $ show $ max "aa" "z"
    log $ show $ findMax (Nil :: List Int)
    log $ show $ findMax (37 : Nil)
    log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
    log $ show $ findMax ("a" : "bbb" : "c" : Nil)
    log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
    log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
