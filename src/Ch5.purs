module Ch5 where

import Prelude (Unit, discard, negate, otherwise, show, max, (+), (-), (/=), (<), (==), (>), (>=), type (~>), (<<<))

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
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
    - head (get the head of a list)
    - tail (get the rest of a list after removed the first element, i.e. the head)
    - last (get the last element of a list)
    - init (get the whole list except the last element)
    - uncons (return a record of head and tail from the list)
    - index (get the element at index in a list) and operator !!
    - findIndex (get the index of an element satisfying a predicate in the list)
    - findLastIndex (get the last index of the elements satisfying a predicate in the list)
    - reverse (reverse a list)
    - concat (takes a List of Lists and returns a single List with all element in the same order)
    - filter (KEEP the elements of a List, which satisfies a certain condition)
    - catMaybes (given a list of Maybe values, filter Nothing and unwrap Just into a List)
    - range (given 2 endpoints, return a List, ascending or descending based on the 2 endpoints)
    - take (take a number of elements from the List, or all elements if not enough)
    - drop (drop a number of elements from the List, or all elements if not enough)
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
length l = go 0 l where
  go acc Nil = acc
  go acc (_ : xs) = go (acc + 1) xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
{-
-- This does not have any useless case, but it is not tail recursive
init (x : xs) = 
  let initRest = init xs in
    case initRest of 
      Just ys -> Just (x : ys)      
      Nothing -> Just Nil
-}
-- This has the first case of local function which will be never called, but made to accommodate the compiler
-- However, it's tail recursive
init l = Just $ go l where
  go Nil = Nil -- This case will be never executed => just to make the compilation passed
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a}
uncons Nil = Nothing
uncons (x : xs) = Just {head: x, tail: xs}

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
{-
-- use guard only
index (x : xs) idx
  | idx < 0 = Nothing
  | idx == 0 = Just x
  | otherwise = index xs (idx - 1)
-}
-- use pattern matching with guard
index _ i | i < 0 = Nothing
index (x : _) 0 = Just x
index (_ : xs) idx = index xs (idx - 1)

-- To facilitate the fact that !! binary operator is used for index, the Int parameter is the 2nd parameter
-- of index function, even though it changes more often than the list parameter
infixr 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex p l = go 0 l where
  go _ Nil = Nothing
  go idx (x : xs)
    | p x = Just idx 
    | otherwise = go (idx + 1) xs 

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex p l = go 0 Nothing l where
  go :: Int -> Maybe Int -> List a -> Maybe Int -- type of local function use the type a defined in the parent function
  go _ Nothing Nil = Nothing
  go _ lastIdx Nil = lastIdx
  go idx lastIdx (x : xs) = go (idx + 1) (if p x then Just idx else lastIdx) xs

reverse :: List ~> List
reverse = go Nil where -- imagine each time we pop the head of the list, we push it to the head of reversed
  go reversed Nil = reversed
  go reversed (x : xs) = go (x : reversed) xs

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: ∀ a. (a -> Boolean) -> List a -> List a
{-
-- Not tail recursive version, this is fast but needs more space (stack for recursion)
filter _ Nil = Nil
filter pred (x : xs) = 
  if pred x then x : filter preed xs 
  else filter pred xs
-}
-- Tail recursive version, slower with 2x passing throught the list, but need less space
filter pred = reverse <<< go Nil where
  go nl Nil = nl
  go nl (x : xs) = if pred x then go (x : nl) xs else go nl xs

catMaybes :: ∀ a. List (Maybe a) -> List a
-- Non tail-recursive version
catMaybes Nil = Nil
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs
{-
-- Tail-recursive version
catMaybes = reverse <<< go Nil where
  go nl Nil = nl
  go nl (Nothing : xs) = go nl xs
  go nl (Just x : xs) = go (x : nl) xs
-}

-- !!! This seems more readable than the solution in the book
range :: Int -> Int -> List Int
range start end = go Nil end where
  go l end'
    | start == end' = start : l
    | otherwise = go (end' : l) (end' - stepBack)
  stepBack = if start < end then 1 else (-1)

take :: ∀ a. Int -> List a -> List a
-- Non tail-recursive version
take n = go (max 0 n) where
  go 0 _ = Nil
  go _ Nil = Nil
  go n' (x : xs) = x : take (n' - 1) xs
{-
-- Tail-recursive but traversing the list twice
take n = reverse <<< go Nil (max 0 n) where
  go nl 0 _ = nl
  go nl _ Nil = nl
  go nl n' (x : xs) = go (x : nl) (n' - 1) xs
-}

drop :: ∀ a. Int -> List a -> List a
drop n = go (max 0 n) where
  go 0 l = l
  go _ Nil = Nil
  go n' (_ : xs) = go (n' - 1) xs 

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
  log $ show (head Nil :: Maybe Unit) -- this helps the compiler to deduce the returned type of head in case of Nil list
  log $ show $ head ("abc" : "123" : Nil)
  log $ show $ tail (Nil :: List Unit) -- this helps the compiler to deduce the type of list elements in case of Nil list
  log $ show $ tail ("abc" : "123" : Nil)
  log $ show (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ index (1 : 2 : 3 : Nil) (-99)
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log $ show $ reverse (Nil :: List Int)
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log $ show $ range 1 10
  log $ show $ range 3 (-3)
  log $ show $ take (-1) (1 : 2 : 3 : Nil)
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop 10 (Nil :: List Unit)