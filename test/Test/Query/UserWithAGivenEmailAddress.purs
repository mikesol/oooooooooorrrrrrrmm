module Test.Query.UserWithAGivenEmailAddress where

import Prelude
import Effect.Aff (Aff)
import Data.Symbol (reflectSymbol)
import Type.Proxy (Proxy(..))
import Foreign (Foreign)
import Yoga.JSON (read, writeImpl, E)
import Data.Maybe (Maybe)


type I = { "$1" :: String }
type Q = """
SELECT * FROM users WHERE email = $1;
"""
type O = Array { id :: Int, email :: Maybe String, first_name :: String, last_name :: String, username :: String, verified :: Boolean, phone_number :: Maybe String, image_url :: Maybe String }
run :: (String -> Foreign -> Aff Foreign) -> I -> Aff (E O)
run go  i = do
  o <- go (reflectSymbol (Proxy :: _ Q)) $ writeImpl ([ writeImpl i."$1"] :: Array Foreign)
  pure $ read o
