module OOOOOOOOOORRRRRRRMM.ConvertToResult where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String as String

convertToResult :: String -> { result :: String, success :: Boolean }
convertToResult input = { result, success }
  where
  split = String.split (String.Pattern "```") input
  result = fromMaybe input do
    afterQuote <- split !! 1
    pure $ String.joinWith "\n" $ Array.drop 1 $ String.split (String.Pattern "\n") afterQuote
  success = Array.length split > 2