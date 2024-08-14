module OOOOOOOOOORRRRRRRMM.Prompts.DoTypescript.DoIoTs where

import Prelude

import Foreign (Foreign)
import Yoga.JSON (writeImpl)

newtype Schema = Schema String
newtype Query = Query String

system :: String
system = """You are a helpful assistant that generates typescript io-ts bindings for SQL queries."""

user :: Schema -> Query -> String
user (Schema schema) (Query query) =
  """A postgres database was created using the following sql:
<sql>
""" <> schema
    <>
      """
</sql>

Against this database, there is the following query:

<query>
"""
    <> query
    <>
      """
</query>

Please generate typescript bindings for this query. The bindings should follow the following template where:

<typescript>

export const i = ... // input type defined using io-ts
export type Q = `"""
    <> query
    <>
      """`;
export const q: Q = `"""
    <> query
    <>
      """`;
export const o = ... // output type
</typescript>

As you can see, the query Q just needs to be quoted verbatim, as Typescript allows for typelevel strings. Ditto for `q`.

For i, this should be a tuple with as many entries as there are input. For example, if the query has wildcards $1 and $2, where $1 is supposed to be a string and $2 is supposed to be a boolean, the input io-ts validator should be:

<typescript>
export const i = t.tuple([t.string, t.boolean]);
</typescript>

For o, this should be an array of objects, each of which represents an entry. For example, if the columns returned are an id string, a verified boolean, and an optional email string, the output io-ts validator should be:

<typescript>
export const o = t.array(
  t.type({ id: t.string, verified: t.boolean, email: t.union([t.null, t.string]) })
);
</typescript>

For each postgres type, here is the equivalent io-ts binding namespaced by t. I'm using a newer version of io-ts with t.date and t.json, so it's legit when needed.

- text -> t.string
- int4 -> t.number
- int8 -> t.number
- bool -> t.boolean
- float4 -> t.number
- float8 -> t.number
- jsonb -> t.json
- json -> t.json
- timestamptz -> t.date
- date -> t.date
- timestamp -> t.date

Arrays should be t.array() of these things. Types that can be `null` types should use a union between the type and t.null.

Please generate the complete Typescript file with nothing extra (success=true). If you can't, please send back why (success=false).
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