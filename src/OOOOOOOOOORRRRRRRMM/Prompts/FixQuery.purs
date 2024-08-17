module OOOOOOOOOORRRRRRRMM.Prompts.FixQuery where

import Prelude

import Data.Maybe (Maybe, maybe)
import Safe.Coerce (coerce)

system :: String
system = """You are a helpful assistant that revises SQL queries based on a previous schema and a current schema."""

newtype Sql = Sql String
newtype Migration = Migration String
newtype Query = Query String
newtype Intention = Intention String
newtype AdditionalContext = AdditionalContext String

user :: Sql -> Migration -> Intention -> Query -> Maybe AdditionalContext -> String
user (Sql oldSql) (Migration migration) (Intention intention) (Query query) additionalContext =
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

Previously, for the old schema, the user had expressed the following intention:

<intention>
""" <> intention <> """
</intention>

Which was translated into the following query:

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
Now, we need to ascertain if the query is still valid _and_ if it still corresponds to the intention. For example, an INSERT statement could be valid, but if a column was added in the new schema and the intention was to insert all columns, then even though the query may be valid, it does not correspond to the intention and we should rewrite it.

If the query does not need to be rewritten, echo back the query in backticks. If it is not, revise the query to be valid with the new schema and return it in backticks. If the query is nonsensical, or if a revision of the query would be too hard because of any ambiguity, return a message that reports the reason the task couldn't be completed.
"""
