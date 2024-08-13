module OOOOOOOOOORRRRRRRMM.Prompts.DoMigration where

import Prelude

import Foreign (Foreign)
import Yoga.JSON (writeImpl)

newtype Sql = Sql String
newtype Ask = Ask String

system :: String
system = """You are a helpful assistant that generates SQL migrations based on a preexisting schema and user input."""

user :: Sql -> Ask -> String
user (Sql sql) (Ask ask) =
  """A postgres database was created using the following sql:
<sql>
""" <> sql
    <>
      """
</sql>

Now, the user has asked to generate a migration for this schema. Below is the user's ask. Please generate the appropriate migration SQL that can be copied verbatim and sent to a DB, and when things are underspecified, use reasonable defaults (success=true). If the ask is too ambiguous, please follow up with a message asking the user to clarify their ask and try again (success = false). If the ask is otherwise nonsensical, please tell the user politely that you don't understand what they want and ask them to revise their question (success=false). When success is true and you do generate a migration, make it as human-readable as possible. Please avoid unnecessary namespacing when not needed (ie if a table is public.user and you don't need the public, don't use it).

<user-ask>
"""
    <> ask
    <>
      """
</user-ask>
"""

responseFormat :: Foreign
responseFormat = writeImpl
  { "type": "json_schema"
  , "json_schema":
      { "name": "migration_response"
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