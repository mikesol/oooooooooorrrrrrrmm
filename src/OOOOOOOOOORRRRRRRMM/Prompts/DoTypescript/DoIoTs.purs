module OOOOOOOOOORRRRRRRMM.Prompts.DoTypescript.DoIoTs where

import Prelude


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
export const q = t.literal(`"""
    <> query
    <>
      """`);
export const o = ... // output type
export const run = (f: (s: string, v: any) => Promise<any>) => (data: t.TypeOf<typeof i>) => f(q.value, [...]).then(x => o.decode(x));
</typescript>

As you can see, the literal q just needs to be quoted verbatim in the io-ts literal.

For i, this should be a t.type with as many entries as there are input. For example, if the query has wildcards $1 and $2, where $1 is supposed to represent an email address of type string and $2 is supposed to represent a verified status of type boolean, the input io-ts validator should be:

<typescript>
export const i = t.type({
  email: t.string, // $1
  verified: t.boolean // $2
});
</typescript>

In the object, the order of the keys _must_ be the order of the positional arguemnts, and there should be a comment after each key indicating its argument.

For o, this should be an array of objects, each of which represents an entry. For example, if the columns returned are an id string, a verified boolean, and an optional email string, the output io-ts validator should be:

<typescript>
export const o = t.array(
  t.type({ id: t.string, verified: t.boolean, email: t.union([t.null, t.string]) })
);
</typescript>

For `run`, the content can be quoted verbatim _except_ for that [...] array, which should be filled in by the input arguments in correct order. In the example above, that'd be

<typescript>
[data.email, data.verified]
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

Please generate the complete Typescript file in backticks. If you can't, please send the reason why.
"""
