module OOOOOOOOOORRRRRRRMM.Prompts.FixQuery where

import Prelude

import Data.Maybe (Maybe, maybe)
import OOOOOOOOOORRRRRRRMM.Completions as C
import OOOOOOOOOORRRRRRRMM.Prompts.NorseGods (norseGods)
import Safe.Coerce (coerce)

system :: Sql -> Migration -> String
system (Sql sql) (Migration migration) =
  """You are a helpful assistant that helps users update their SQL queries after a recent migration. Before the migration, the schema used to be:

```sql
""" <> sql <> norseGods
    <>
      """
```

Then, this migraiton was executed:

```sql
-- First part of the migration
-- Step 1: Add the new publication_date column
ALTER TABLE obscure_norse_god_research_papers
ADD COLUMN publication_date DATE;

-- Step 2: Migrate the data from publication_year to publication_date
UPDATE obscure_norse_god_research_papers
SET publication_date = TO_DATE(publication_year::TEXT, 'YYYY');

-- Step 3: Drop the old publication_year column
ALTER TABLE obscure_norse_god_research_papers
DROP COLUMN publication_year;

-- Second part of the migration
ALTER TABLE norse_god_battles
DROP COLUMN battle_date;

-- Third part of the migration
"""
    <> migration
    <>
      """
```

Developers are understandably concerned that their queries may no longer be correct or may not fully correspond to their original intentions. Your job is to help them update their queries to match the new schema or their original intentions. If the query is still correct, let them know. If it needs updating, help them update it. If you need more information to do so, ask for it.
"""

newtype Sql = Sql String
newtype Migration = Migration String
newtype Query = Query String
newtype Intention = Intention String
newtype AdditionalContext = AdditionalContext String

loremIpsum :: Array C.Message
loremIpsum =
  [ C.message C.user
      """Hey! A while ago, I asked you to generate a postgres query based on the following prompt:

```prompt
Get all anthropologists' full names and the publication years of their research papers on obscure Norse gods.
```

You sent me back the following query:

```sql
SELECT a.full_name, p.publication_year
FROM anthropologists_researching_ancient_norse_gods a
JOIN obscure_norse_god_research_papers p ON a.id = p.anthropologist_id;
```

However, the DB has migrated since and I'm concerned that the query may be either incorrect or many not fully correspond to my original intention. Could you update it if it no longer corresponds to the db schema or to my original wishes? If nothing needs updating, just let me know.
"""
  , C.message C.assistant
      """To update this query for the new schema where `publication_year` has been replaced by `publication_date`, you would adjust the query as follows:

```sql
SELECT a.full_name, EXTRACT(YEAR FROM p.publication_date) AS publication_year
FROM anthropologists_researching_ancient_norse_gods a
JOIN obscure_norse_god_research_papers p ON a.id = p.anthropologist_id;
```

Explanation:

- The `EXTRACT(YEAR FROM p.publication_date)` function extracts the year from the publication_date column, effectively replacing the old `publication_year` column.
- This query now works with the updated schema, ensuring compatibility with your new database structure.

You can use this new query in your applications or reports to continue retrieving the same information in a way that's compatible with the updated schema.
"""
  , C.message C.user
      """Hey! A while ago, I asked you to generate a postgres query based on the following prompt:

```prompt
I want to get a list of all battles fought by the Norse gods, including the name of the battle and the date it took place. How do I write that query?
```

You sent me back the following query:

```sql
SELECT battle_name, battle_date
FROM norse_god_battles;
```

However, the DB has migrated since and I'm concerned that the query may be either incorrect or many not fully correspond to my original intention. Could you update it if it no longer corresponds to the db schema or to my original wishes? If nothing needs updating, just let me know.
"""
  , C.message C.assistant
      """It seems that the `battle_date` column has been removed from the `norse_god_battles` table, so we can no longer retrieve the date of each battle. However, you can still get a list of all the battles by their names.

Here's the updated query:

```sql
SELECT battle_name
FROM norse_god_battles;
```

This query will give you a list of all battle names, but without the battle dates since that information is no longer available in the schema. If you need to track battle dates again in the future, you may want to consider adding a new column or table to store that information.
"""
  , C.message C.user
      """Hey! A while ago, I asked you to generate a postgres query based on the following prompt:

```prompt
I want to find out which Norse god has the most worshipers. How would I write that query?
```

You sent me back the following query:

```sql
SELECT name, worshipers_count
FROM favorite_ancient_norse_gods
ORDER BY worshipers_count DESC
LIMIT 1;
```

However, the DB has migrated since and I'm concerned that the query may be either incorrect or many not fully correspond to my original intention. Could you update it if it no longer corresponds to the db schema or to my original wishes? If nothing needs updating, just let me know.
"""
  , C.message C.assistant
      """Your query is still valid! Here it is for reference:

```sql
SELECT name, worshipers_count
FROM favorite_ancient_norse_gods
ORDER BY worshipers_count DESC
LIMIT 1;
```

Please let me know if you have any more questions regarding queries that may need updating.
"""

  ]

user :: Intention -> Query -> Maybe AdditionalContext -> String
user (Intention intention) (Query query) additionalContext =
  """Hey! A while ago, I asked you to generate a postgres query based on the following prompt:

```prompt
""" <> intention
    <>
      """
```

You sent me back the following query:

```sql
"""
    <> query
    <>
      """
```

However, the DB has migrated since and I'm concerned that the query may be either incorrect or many not fully correspond to my original intention. Could you update it if it no longer corresponds to the db schema or to my original wishes? If nothing needs updating, just let me know.
"""
    <> maybe ""
      ( \c ->
          """Also:
""" <> c <>
            """
"""
      )
      (coerce additionalContext)
