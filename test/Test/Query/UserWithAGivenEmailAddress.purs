module Test.Query.UserWithAGivenEmailAddress where

import Prelude
import Effect.Aff (Aff)
import Data.Symbol (reflectSymbol)
import Type.Proxy (Proxy(..))
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)
import Data.Date (Date)
import Data.Maybe (Maybe)


type I = { "$1" :: String }
type Q = """
SELECT * FROM users WHERE email = $1;
"""
type O = Array { id :: Int, email :: Maybe String, first_name :: String, last_name :: String, username :: String, verified :: Boolean, phone_number :: Maybe String, image_url :: Maybe String, date_joined :: Date, settings :: Maybe Foreign }
run :: (String -> Foreign -> Aff Foreign) -> I -> Aff O
run go  i = do
  o <- go (reflectSymbol (Proxy :: _ Q)) $ unsafeCoerce ([ unsafeCoerce i."$1"] :: Array Foreign)
  pure $ unsafeCoerce o
