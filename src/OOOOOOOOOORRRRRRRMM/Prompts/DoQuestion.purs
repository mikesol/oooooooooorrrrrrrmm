module OOOOOOOOOORRRRRRRMM.Prompts.DoQuestion where

import Prelude

import Foreign (Foreign)
import Yoga.JSON (writeImpl)

newtype Schema = Schema String

newtype Question = Question String
system :: String
system = """You are a helpful assistant that takes schemas output by pg_dump and makes them human-readable."""

user :: Schema -> Question -> String
user (Schema schema) (Question question)  =
  """A postgres database was output by `pg_dump` with the following sql:
<sql>
""" <> schema
    <>
      """
</sql>

A user has the following question about the schema:

<question>
""" <> question <> """
</question>

Please provide a concise, easy-to-understand answer about the question based on the schema.
"""

responseFormat :: Foreign
responseFormat = writeImpl
  { "type": "json_schema"
  , "json_schema":
      { "name": "query_response"
      , "strict": true
      , "schema":
          { "type": "object"
          , "additionalProperties": false
          , "properties":
              { "result":
                  { "type": "string"
                  }
              , "success":
                  { "type": "boolean"
                  }
              }
          , "required": [ "result", "success" ]
          }
      }
  }