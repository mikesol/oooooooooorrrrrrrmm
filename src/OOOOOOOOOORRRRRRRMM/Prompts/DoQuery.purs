module OOOOOOOOOORRRRRRRMM.Prompts.DoQuery where

import Prelude

import OOOOOOOOOORRRRRRRMM.Completions as C
import OOOOOOOOOORRRRRRRMM.Prompts.NorseGods (norseGods)

newtype Sql = Sql String
newtype Ask = Ask String

system :: Sql -> String
system (Sql sql) =
  """You are a helpful assistant that helps users generate postgres SQL queries against the following schema:

```sql
""" <> sql <> norseGods
    <>
      """
```

When responding to a user, use correctly-numbered positional parameters (ie $1, $2, etc). For example, when there's language like "for a given X" or "for a X" or "for an X chosen by the user", use a positional parameter. If the user's request is too ambiguous, please follow up with a message asking the user to clarify their ask and try again. If the ask is otherwise nonsensical, please tell the user politely that you don't understand what they want and ask them to revise their question. When success is true and you do generate a query, make it as human-readable as possible. Please avoid unnecessary namespacing when not needed. For example, if a table is public.user and you don't need the public, don't use it. If the user provides raw SQL, they are giving you their own query. In this case, echo it back verbatim if it is consistent with the schema, modify it slightly if there are forgivable inconsistencies, and fail if you can't get it in line with the existing schema.
"""

loremIpsum :: Array C.Message
loremIpsum =
  [ C.message C.user "I want to get a list of all the names of the ancient Norse gods."
  , C.message C.assistant
      """Sure! Here’s a query that will retrieve all the names of the ancient Norse gods from the favorite_ancient_norse_gods table:

```sql
SELECT name
FROM favorite_ancient_norse_gods;
```

This query selects only the `name` column from the `favorite_ancient_norse_gods` table, giving you a list of all the Norse gods' names.

"""
  , C.message C.user "I want to find out which anthropologists specialize in researching Thor. How can I write a query to do that?"
  , C.message C.assistant
      """You can use a `JOIN` to combine the `favorite_ancient_norse_gods` table with the `anthropologists_researching_ancient_norse_gods` table. Here’s the query:

```sql
SELECT a.full_name, a.area_of_expertise
FROM anthropologists_researching_ancient_norse_gods a
JOIN favorite_ancient_norse_gods g ON a.favorite_god_id = g.id
WHERE g.name = 'Thor';
```

This query selects the full names and areas of expertise of anthropologists who specialize in researching Thor.
"""
  , C.message C.user """--raw
SELECT name, domain
FROM favorite_ancient_norse_gods;
"""
  , C.message C.assistant """That looks like a valid query! Here's the query you provided:

```sql
SELECT name, domain
FROM favorite_ancient_norse_gods;
```
"""
  , C.message C.user "Could you give me a list of all unique ids?"
  , C.message C.assistant """Could you clarify which table you are referring to?"""
  , C.message C.user "Sorry, could you give me a list of all unique ids in favorite_ancient_norse_gods?"
  , C.message C.assistant
      """Sure! Here’s a query that will retrieve all the unique ids from the `favorite_ancient_norse_gods` table:
SELECT DISTINCT id
FROM favorite_ancient_norse_gods;
```

This query selects all unique IDs from the id column in the favorite_ancient_norse_gods table. Since id is typically a unique identifier (especially if it's a primary key), each value should already be unique, so using DISTINCT might not change the result, but it ensures no duplicates if there were any.
"""
  , C.message C.user "I want to know the total number of worshipers for all the gods combined in the Gods table. How do I write that query?"
  , C.message C.assistant
      """You can use the SUM function to calculate the total number of worshipers. Here’s the query:

```sql
SELECT SUM(worshipers_count) AS total_worshipers
FROM favorite_ancient_norse_gods;
```

This query sums up all the values in the `worshipers_count` column and returns the total number of worshipers across all gods.
"""
  , C.message C.user """--raw
SELECT name, worshipers_count
favorite_ancient_norse_gods;
"""
  , C.message C.assistant """It looks like you're missing a `FROM` clause in your query. It looks like the query is missing the FROM keyword, which specifies the table you're selecting from. Here's the corrected query:

```sql
SELECT name, worshipers_count
FROM favorite_ancient_norse_gods;
```
"""
  ]

user :: Ask -> String
user (Ask ask) = ask
