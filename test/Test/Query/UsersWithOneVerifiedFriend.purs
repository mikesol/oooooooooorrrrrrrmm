module Test.Query.UsersWithOneVerifiedFriend where

import Prelude
import Effect.Aff (Aff)
import Data.Symbol (reflectSymbol)
import Type.Proxy (Proxy(..))
import Foreign (Foreign)
import Yoga.JSON (read, writeImpl, E)
import Data.Maybe (Maybe)



type Q = """
SELECT DISTINCT u.* FROM users u JOIN friends f ON u.id = f.asker OR u.id = f.receiver;
"""
type O = Array { id :: Int, email :: Maybe String, first_name :: String, last_name :: String, username :: String, verified :: Boolean, phone_number :: Maybe String, image_url :: Maybe String }
run :: (String -> Foreign -> Aff Foreign) -> Aff (E O)
run go = do
  o <- go (reflectSymbol (Proxy :: _ Q)) $ writeImpl ([ ] :: Array Foreign)
  pure $ read o
