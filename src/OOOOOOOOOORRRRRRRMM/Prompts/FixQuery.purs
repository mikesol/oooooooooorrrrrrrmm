module OOOOOOOOOORRRRRRRMM.Prompts.FixQuery where

import Prelude

import Data.Maybe (Maybe, maybe)
import Foreign (Foreign)
import Safe.Coerce (coerce)
import Yoga.JSON (writeImpl)

system :: String
system = """You are a helpful assistant that revises SQL queries based on a previous schema and a current schema."""

newtype Sql = Sql String
newtype Migration = Migration String
newtype Query = Query String
newtype AdditionalContext = AdditionalContext String

user :: Sql -> Migration -> Query -> Maybe AdditionalContext -> String
user (Sql oldSql) (Migration migration) (Query query) additionalContext =
  """A postgres database was created using the following sql:

<old-sql>
""" <> oldSql
    <>
      """
</old-sql>

Then, the user migrated it with this migration:

<migration>
"""
    <> migration
    <>
      """
</migration>

Previously, for the old schema, the user had written this query:

<query>
"""
    <> query
    <>
      """
</query>
"""
    <> maybe ""
      ( \c ->
          """

The user has also given the following additional context to help with the revision.
<context>
""" <> c <>
            """
</context>
"""
      )
      (coerce additionalContext)
    <>
      """
Now, we need to ascertain if the query is still valid. If it is, echo back the query, success=true and revised=false. If it is not, revise the query to be valid with the new schema and return it with success=true and revised=true. If the query is nonsensical, return success=false and revised=false. If the revision of the query would be too hard because of any ambiguity, return success=false and revised=true with a message that reports the reason the task couldn't be completed.
"""

responseFormat :: Foreign
responseFormat = writeImpl
  { "type": "json_schema"
  , "json_schema":
      { "name": "fix_query_response"
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
              , "revised":
                  { "type": "boolean"
                  }
              }
          , "required": [ "result", "success", "revised" ]
          }
      }
  }