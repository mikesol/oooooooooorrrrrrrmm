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

For I, this should be a record with as many entries as there are positional parameters. The key of each entry should be human-readable and not duplicated, and the value should be the correct type. For example, if the query has positional parameters $1 and $2, where $1 is supposed to be a string for email and $2 is supposed to be a boolean for verified, the input type could be:

<purescript>
type I =
  { email :: String -- $1
  , verified :: Boolean -- $2
  }
</purescript>

Each key/value pair of the input must _always_ be on its own line and _always_ be followed by a comment indicating the positional parameter, and the record must always be defined in the order of the parameters.

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
- timestamptz -> JSDate
- date -> JSDate
- timestamp -> JSDate

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