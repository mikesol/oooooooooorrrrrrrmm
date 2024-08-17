module OOOOOOOOOORRRRRRRMM.Prompts.DoQuery where

import Prelude


newtype Sql = Sql String
newtype Ask = Ask String

system :: String
system = """You are a helpful assistant that generates SQL queries based on a preexisting schema and user input."""

user :: Sql -> Ask -> String
user (Sql sql) (Ask ask) =
  """A postgres database was created using the following sql:
<sql>
""" <> sql
    <>
      """
</sql>

Now, the user has asked to generate a query based on this schema. Below is the user's ask. Please generate the appropriate query SQL in backticks. Unless you are matching against a specific constant, use correctly-numbered positional parameters. For example, when there's language like "for a given X" or "for a X" or "for an X chosen by the user", use positional parameters. When you generate a query, make it as human-readable as possible. Please avoid unnecessary namespacing when not needed (ie if a table is public.user and you don't need the public, don't use it). If the user's ask is raw SQL, they are providing their own query. In this case, echo it back verbatim if it is consistent with the schema, modify it slightly if there are forgivable inconsistencies. On the other hand, if you can't produce a query because the user's request would violate the schema or it is ambiguous or nonsensical, please tell the user politely what you're struggling with and ask them to revise their question.

<user-ask>
"""
    <> ask
    <>
      """
</user-ask>
"""
