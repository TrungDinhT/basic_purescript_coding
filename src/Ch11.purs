module Ch11 where

import Data.List (List(..), (:), singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
-- import Data.Semigroup.Foldable (foldl1) -- not import this to implement ourselves
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Ord (class Ord)
import Data.Semiring (class Semiring, zero)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), discard, negate, show, ($), (+), (>), (<>), (<<<))


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
findMaxNE (NonEmptyList (head :| tail)) = foldl max head tail
-}
-- Using foldl1
findMaxNE (NonEmptyList l) = foldl1 max l

-- foldl1
foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
{-
-- Pattern matching with NonEmpty data constructor
foldl1 f (NonEmpty x xs) = foldl f x xs
-}
foldl1 f (x :| xs) = foldl f x xs 


-- sum
sum :: ∀ f a. Foldable f => Semiring a => f a -> a
{-
-- Manually with recursive
sum Nil = 0
sum (x : xs) = x + sum xs
-- Manually with tail recursive
sum = go 0 where
    go acc Nil = acc
    go acc (x : xs) = go (acc + x) xs
-}
-- Use foldl
sum = foldl (+) zero


-- Tree data type
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- toList to traverse the tree
toList :: Tree ~> List
toList (Leaf value) = singleton value 
toList (Node left right) = toList left <> toList right

-- Foldable for Tree
instance foldableTree :: Foldable Tree where
    foldr f acc = foldr f acc <<< toList
    foldl f acc = foldl f acc <<< toList
    foldMap f = foldMap f <<< toList


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
    log $ show $ sum (1 : 2 : 3 : Nil)
    log $ show $ sum (1.1 : 2.2 : 3.3 : Nil)
    log $ show $ sum [1, 2, 3]
    log $ show $ sum [1.1, 2.2, 3.3]
    log $ show $ toList (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
    log $ show $ sum (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
