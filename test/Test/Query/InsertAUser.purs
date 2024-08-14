module Test.Query.InsertAUser where

import Prelude
import Effect.Aff (Aff)
import Data.Symbol (reflectSymbol)
import Type.Proxy (Proxy(..))
import Foreign (Foreign)
import Yoga.JSON (writeImpl)
import Data.Maybe (Maybe)


type I = { "$1" :: String, "$2" :: String, "$3" :: String, "$4" :: String, "$5" :: Boolean, "$6" :: Maybe String, "$7" :: Maybe String }
type Q = """
INSERT INTO users (email, first_name, last_name, username, verified, phone_number, image_url) VALUES ($1, $2, $3, $4, $5, $6, $7);
"""

run :: (String -> Foreign -> Aff Foreign) -> I -> Aff Unit
run go  i = do
  void $ go (reflectSymbol (Proxy :: _ Q)) $ writeImpl ([ writeImpl i."$1", writeImpl i."$2", writeImpl i."$3", writeImpl i."$4", writeImpl i."$5", writeImpl i."$6", writeImpl i."$7" ] :: Array Foreign)
