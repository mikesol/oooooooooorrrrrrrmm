module OOOOOOOOOORRRRRRRMM.Prompts.DoPurescript where

import Prelude

import Foreign (Foreign)
import Yoga.JSON (writeImpl)

newtype Schema = Schema String
newtype Query = Query String
newtype ModuleName = ModuleName String 
system :: String
system = """You are a helpful assistant that generates purescript bindings for SQL queries."""

user :: Schema -> Query -> ModuleName -> String
user (Schema schema) (Query query) (ModuleName moduleName)  =
  """A postgres database was created using the following sql:
<sql>
""" <> schema
    <>
      """
</sql>

Against this database, there is the following query:

<query>
""" <>query  <> """
</query>

Please generate PureScript bindings for this query. The bindings should follow the following template where:

<purescript>
module """ <> moduleName <> """ where

type I = ... -- input type
type Q = """<> "\"\"\"\n" <> query <>"\n\"\"\"" <> """
type O = ... -- output type

</purescript>

As you can see, the query Q just needs to be quoted verbatim, as PureScript allows for typelevel strings.

For I, this should be a record with as many entries as there are input, with the same labels as in the query. For example, if the query has wildcards $1 and $2, where $1 is supposed to be a string and $2 is supposed to be a boolean, the input type should be:

<purescript>
type I = { "$1" :: String, "$2" :: Boolean }
</purescript>

Note that the input type's keys _must_ be quoted.

If there is no input, the type of I _must_ be {}.

For O, this should be an array of rows, each of which represents an entry. For example, if the columns returned are an id string, a verified boolean, and an optional email string, the output type should be:

<purescript>
type O = Array { id :: String, verified :: Boolean, email :: Maybe String }
</purescript>

If there is no output, the type of O _must_ be {}.

For each postgres type, here is the equivalent PureScript type:

- text -> String
- int4 -> Int
- int8 -> Int
- bool -> Boolean
- float4 -> Number
- float8 -> Number
- jsonb -> Foreign
- json -> Foreign
- timestamptz -> Date
- date -> Date
- timestamp -> Date

Arrays should be arrays of these things. Stuff that can be `null` types should be Maybe in I and O.

Please generate the complete PureScript file with nothing extra (success=true). Do not put it in backticks or xml, just the raw PureScript file. If you cant, please send back why (success=false).
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