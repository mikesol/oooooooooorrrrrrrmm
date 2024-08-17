module OOOOOOOOOORRRRRRRMM.Prompts.DoMigration where

import Prelude


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

Now, the user has asked to generate a migration for this schema. Below is the user's ask. Please generate the appropriate migration SQL in backticks. When you generate a migration, make it as human-readable as possible. Please avoid unnecessary namespacing when not needed (ie if a table is public.user and you don't need the public, don't use it). If the ask is too ambiguous, please follow up with a message asking the user to clarify their ask and try again. If the ask is otherwise nonsensical, please tell the user politely that you don't understand what they want and ask them to revise their question.

<user-ask>
"""
    <> ask
    <>
      """
</user-ask>
"""
