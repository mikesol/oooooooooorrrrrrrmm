module OOOOOOOOOORRRRRRRMM.PGInfo where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign (ForeignError(..), fail)
import Foreign.Object (Object)
import Yoga.JSON (class ReadForeign, readImpl)

-- Define an ADT for PostgreSQL data types
data PostgresType
  = PGInteger
  | PGBoolean
  | PGTimestamp
  | PGTimestampz
  | PGJson
  | PGText
  | PGVarchar
  | PGNumeric
  | PGUuid
  | PGBytea
  | PGDate
  | PGTime
  | PGTimetz
  | PGInterval
  | PGRecord
  | PGEnum
  | PGTsvector
  | PGTsquery

derive instance Eq PostgresType

-- Define a type for input parameters
newtype InputParameter = InputParameter
  { type :: PostgresType
  , is_array :: Boolean
  , is_nullable :: Boolean
  , position_starting_from_1 :: Int
  }

derive instance Newtype InputParameter _

derive newtype instance ReadForeign InputParameter

-- Define a type for output columns
newtype OutputColumn = OutputColumn
  { type :: PostgresType
  , is_array :: Boolean
  , is_nullable :: Boolean
  }

derive instance Newtype OutputColumn _

derive newtype instance ReadForeign OutputColumn

-- Define the schema for the query
newtype PostgresQuerySchema = PostgresQuerySchema
  { input :: Object InputParameter
  , output :: Object OutputColumn
  }

derive instance Newtype PostgresQuerySchema _
derive newtype instance ReadForeign PostgresQuerySchema

-- Function to map a string to a Maybe PostgresType
stringToPostgresType :: String -> Maybe PostgresType
stringToPostgresType str =
  case str of
    "integer" -> Just PGInteger
    "boolean" -> Just PGBoolean
    "timestamp" -> Just PGTimestamp
    "timestampz" -> Just PGTimestampz
    "json" -> Just PGJson
    "text" -> Just PGText
    "varchar" -> Just PGVarchar
    "numeric" -> Just PGNumeric
    "uuid" -> Just PGUuid
    "bytea" -> Just PGBytea
    "date" -> Just PGDate
    "time" -> Just PGTime
    "timetz" -> Just PGTimetz
    "interval" -> Just PGInterval
    "record" -> Just PGRecord
    "enum" -> Just PGEnum
    "tsvector" -> Just PGTsvector
    "tsquery" -> Just PGTsquery
    _ -> Nothing

instance ReadForeign PostgresType where
  readImpl i = do
    s <- readImpl i
    case stringToPostgresType s of
      Just pgType -> pure pgType
      Nothing -> fail $ ForeignError $ "Invalid PostgresType: " <> s
