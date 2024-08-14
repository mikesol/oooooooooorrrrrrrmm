module Test.Query.InsertAUser where

import Prelude
import Effect.Aff (Aff)
import Data.Symbol (reflectSymbol)
import Type.Proxy (Proxy(..))
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)
import Data.Date (Date)
import Data.Maybe (Maybe)


type I = {
  "$1" :: Int,
  "$2" :: Maybe String,
  "$3" :: String,
  "$4" :: String,
  "$5" :: String,
  "$6" :: Boolean,
  "$7" :: Maybe String,
  "$8" :: Maybe String,
  "$9" :: Maybe Date,
  "$10" :: Maybe Foreign
}
type Q = """
INSERT INTO users (id, email, first_name, last_name, username, verified, phone_number, image_url, date_joined, settings) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10);
"""

run :: (String -> Foreign -> Aff Foreign) -> I -> Aff Unit
run go  i = do
  void $ go (reflectSymbol (Proxy :: _ Q)) $ unsafeCoerce ([ unsafeCoerce i."$1", unsafeCoerce i."$2", unsafeCoerce i."$3", unsafeCoerce i."$4", unsafeCoerce i."$5", unsafeCoerce i."$6", unsafeCoerce i."$7", unsafeCoerce i."$8", unsafeCoerce i."$9", unsafeCoerce i."$10" ] :: Array Foreign)
