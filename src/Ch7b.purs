module Ch7b where

import Prelude

import Effect (Effect)
import Effect.Console (log)

{-
 - The idea here is simply that we want to encode any kind of data into Comma-Separated-Values format, which is
    - primitives -> string
    - record -> separated values per field (only for field which is not another composite type)
    - array, list ??
-}
newtype CSV = CSV String

test :: Effect Unit
test = do
    log "placeholder"