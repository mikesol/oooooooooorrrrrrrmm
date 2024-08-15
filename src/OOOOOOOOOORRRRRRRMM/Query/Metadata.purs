module  OOOOOOOOOORRRRRRRMM.Query.Metadata where

import Data.Maybe (Maybe)
import Yoga.JSON (class ReadForeign, class WriteForeign)

newtype Metadata = Metadata
  { query_text_checksum :: String
  , result_checksum :: String
  , most_recent_migration_checksum :: String
  , typescript_binding ::
      Maybe
        { query_text_checksum :: String
        , result_checksum :: String
        }
  , purescript_binding ::
      Maybe
        { query_text_checksum :: String
        , result_checksum :: String
        }
  , meta_version :: Int
  }

derive newtype instance ReadForeign Metadata
derive newtype instance WriteForeign Metadata
