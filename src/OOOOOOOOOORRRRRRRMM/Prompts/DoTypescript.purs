module OOOOOOOOOORRRRRRRMM.Prompts.DoTypescript where

import Prelude


newtype Schema = Schema String
newtype Query = Query String
system :: String
system = """You are a helpful assistant that generates typescript bindings for SQL queries."""

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

export type I = ... // input type
export type Q = `"""<> query <>  """`;
export const q: Q = `"""<> query <>  """`;
type O = ... // output type
</typescript>

As you can see, the query Q just needs to be quoted verbatim, as Typescript allows for typelevel strings. Ditto for `q`.

For I, this should be an object with as many entries as there are input and logically-named keys. For example, if the query has wildcards $1 and $2, where $1 is supposed to represent an email of type string and $2 is supposed to represent a verified status of type boolean, the input type could be:

<typescript>
export type I = {
  email: string; // $1
  verified: boolean; // $2
};
</typescript>

The entries in I _must_ be in the same order as the positional parameters in the query, and there should be a comment after each entry with the positional argument.

For O, this should be an array of rows, each of which represents an entry. For example, if the columns returned are an id string, a verified boolean, and an optional email string, the output type should be:

<typescript>
export type O = { id: string, verified: boolean, email?: string }[];
</typescript>

For each postgres type, here is the equivalent Typescript type:

- text -> string
- int4 -> number
- int8 -> number
- bool -> boolean
- float4 -> number
- float8 -> number
- jsonb -> any
- json -> any
- timestamptz -> Date
- date -> Date
- timestamp -> Date

Arrays should be arrays of these things. Stuff that can be `null` types should be optional types of these things.

Please generate the complete Typescript file in backticks. If you can't, please send the reason why.
"""
