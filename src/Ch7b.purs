module Ch7b where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

{-
 - The idea here is simply that we want to encode any kind of data into Comma-Separated-Values format, which is
    - primitives -> string
    - record -> separated values per field (only for field which is not another composite type)
    - array, list ??
-}
newtype CSV = CSV String

class ToCSV a where
    toCSV :: a -> CSV

newtype FullName = FullName String

newtype Age = Age Int

data Occupation = Doctor | Dentist | Lawyer | Unemployed

data Person = Person
    {
        name :: FullName,
        age :: Age,
        occupation :: Occupation
    }

-- derive newtype instance showFullName :: Show FullName -- this is not desired, because it will print the representation of string: \"<string_content>\"
instance showFullName :: Show FullName where
    show (FullName fullname) = fullname

derive newtype instance showAge :: Show Age

derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
    show = genericShow

instance toCSVPerson :: ToCSV Person where
    toCSV (Person { name, age, occupation }) = CSV  $ show name <> "," <> show age <> "," <> show occupation

--Test Data
me :: Person
me = Person {
    name: FullName "Trung",
    age: Age 25,
    occupation: Doctor
}

--Test helpers
derive newtype instance eqCSV :: Eq CSV

test :: Effect Unit
test = do
    log $ show $ toCSV me == CSV "Trung,25,Doctor"