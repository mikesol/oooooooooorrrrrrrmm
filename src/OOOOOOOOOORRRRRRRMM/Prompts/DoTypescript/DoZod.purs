module OOOOOOOOOORRRRRRRMM.Prompts.DoTypescript.DoZod where

import Prelude

import Foreign (Foreign)
import Yoga.JSON (writeImpl)

newtype Schema = Schema String
newtype Query = Query String
system :: String
system = """You are a helpful assistant that generates typescript zod bindings for SQL queries."""

user :: Schema -> Query -> String
user (Schema schema) (Query query)=
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

Please generate typescript bindings for this query. The bindings should follow the following template where:

<typescript>

export const i = ... // input type defined using zod
export type Q = `"""<> query <>  """`;
export const q: Q = `"""<> query <>  """`;
export const o = ... // output type
</typescript>

As you can see, the query Q just needs to be quoted verbatim, as Typescript allows for typelevel strings. Ditto for `q`.

For i, this should be a tuple with as many entries as there are input. For example, if the query has wildcards $1 and $2, where $1 is supposed to be a string and $2 is supposed to be a boolean, the input zod validator should be:

<typescript>
export const i = z.tuple([z.string(), z.boolean()]);
</typescript>

For o, this should be an array of objects, each of which represents an entry. For example, if the columns returned are an id string, a verified boolean, and an optional email string, the output zod validator should be:

<typescript>
export const  o = z.array(z.object({ id: z.string(), verified: z.boolean(), email: z.string().nullable() }));
</typescript>

For each postgres type, here is the equivalent zod binding namespaced by z. I'm using a newer version of zod with z.json(), so it's legit when needed.

- text -> z.string()
- int4 -> z.number()
- int8 -> z.number()
- bool -> z.boolean()
- float4 -> z.number()
- float8 -> z.number()
- jsonb -> z.json()
- json -> z.json()
- timestamptz -> z.date()
- date -> z.date()
- timestamp -> z.date()

Arrays should be z.array() of these things. Types that can be `null` types should have .nullable() appended.

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