module Ch7b where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Data.String (Pattern(..), split)

{-
 - The idea here is simply that we want to encode any kind of data into Comma-Separated-Values format, which is
    - primitives -> string
    - record -> separated values per field (only for field which is not another composite type)
    - array, list ??
-}

-- Defining CSV as a newtype for String
newtype CSV = CSV String
derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

-- ToCSV Typeclass to encode type a to CSV
class ToCSV a where
    toCSV :: a -> CSV

-- FromCSV Typeclass to decode CSV into type a
-- This can be failed, so the method returns Maybe a
class FromCSV a where
    fromCSV :: CSV -> Maybe a

-------------------------------------- Test type: Person --------------------------------------
newtype FullName = FullName String
derive newtype instance eqFullName :: Eq FullName
-- derive newtype instance showFullName :: Show FullName -- this is not desired, because it will print the representation of string: \"<string_content>\"
instance showFullName :: Show FullName where
    show (FullName fullname) = fullname

newtype Age = Age Int
derive newtype instance eqAge :: Eq Age
derive newtype instance showAge :: Show Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance eqOccupation :: Eq Occupation
derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
    show = genericShow

data Person = Person
    {
        name :: FullName,
        age :: Age,
        occupation :: Occupation
    }
derive instance eqPerson :: Eq Person

-------------------------------------- Implementation of toCSV instance for Person --------------------------------------
instance toCSVPerson :: ToCSV Person where
    toCSV (Person { name, age, occupation }) = CSV  $ show name <> "," <> show age <> "," <> show occupation

-------------------------------------- Implementation of fromCSV instance for Person --------------------------------------
toOccupation :: String -> Maybe Occupation
toOccupation "Doctor" = Just Doctor
toOccupation "Dentist" = Just Dentist
toOccupation "Lawyer" = Just Lawyer
toOccupation "Unemployed" = Just Unemployed
toOccupation _ = Nothing

instance fromCSVPerson :: FromCSV Person where
    fromCSV (CSV str) = case split (Pattern ",") str of
        [fullname, age, occupation] -> go' (fromString age) (toOccupation occupation) where
            go' Nothing _ = Nothing
            go' _ Nothing = Nothing
            go' (Just ageValue) (Just occupationValue) = Just $ Person {
                name: FullName fullname,
                age: Age ageValue,
                occupation: occupationValue
            }
        _ -> Nothing

-------------------------------------- Test codes --------------------------------------
test :: Effect Unit
test = do
    let me = Person {
        name: FullName "Thanh Trung Dinh",
        age: Age 25,
        occupation: Doctor
    }
    log $ show $ toCSV me
    log $ show $ toCSV me == CSV "Thanh Trung Dinh,25,Doctor"
    log $ show $ (toCSV me # fromCSV) == Just me