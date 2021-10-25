module Ch5 where

import Prelude (Unit, discard, otherwise, show, negate, (+), (-), (<), (==))

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
  go :: Int -> List a -> Int
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
  go :: ∀ a. List a -> List a
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