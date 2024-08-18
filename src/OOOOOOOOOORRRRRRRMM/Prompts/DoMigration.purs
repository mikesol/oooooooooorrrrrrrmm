module OOOOOOOOOORRRRRRRMM.Prompts.DoMigration where

import Prelude

import OOOOOOOOOORRRRRRRMM.Completions as C
import OOOOOOOOOORRRRRRRMM.Prompts.NorseGods (norseGods)

newtype Sql = Sql String
newtype Ask = Ask String

system :: Sql -> String
system (Sql sql) =
  """You are a helpful assistant that generates postgres SQL migrations based on a preexisting schema and user input. Here's the schema:

```sql
""" <> sql <> norseGods
    <>
      """
```

When generating a migration:

1. Please use only valid postgres.
2. Make it as human-readable as possible.
3. Avoid unnecessary namespacing when not needed. For example, if a table is public.user and you don't need the public, don't use it.
4. If the prompt is too ambiguous, please follow up with a message asking the user to clarify their prompt and try again.
5. If the prompt is otherwise nonsensical, please tell the user politely that you don't understand what they want and ask them to revise their question.
"""

loremImpsum :: Array C.Message
loremImpsum =
  [ C.message C.user "I'd like to add a new column last_updated to the Norse gods table to track when a god's record was last updated. Could you provide a migration for this?"
  , C.message C.assistant
      """Certainly! Here’s the migration script to add the last_updated column to the `favorite_ancient_norse_gods` table:

```sql
ALTER TABLE favorite_ancient_norse_gods
ADD COLUMN last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP;

CREATE OR REPLACE FUNCTION update_favorite_ancient_norse_gods_last_updated_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.last_updated = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_favorite_ancient_norse_gods_last_updated_trigger
BEFORE UPDATE ON favorite_ancient_norse_gods
FOR EACH ROW
EXECUTE FUNCTION update_favorite_ancient_norse_gods_last_updated_column();
``` 
"""
  , C.message C.user "I need to rename the `specialty` column in the anthropologists table to area_of_expertise. Can you help me with the migration?"
  , C.message C.assistant
      """Certainly! Here’s the migration script to rename the specialty column to area_of_expertise:

```sql
ALTER TABLE anthropologists_researching_ancient_norse_gods
RENAME COLUMN specialty TO area_of_expertise;
```
"""
  , C.message C.user
      """--raw
ALTER TABLE airlines_friendly_to_anthropologists
ALTER COLUMN discount_percentage TYPE DECIMAL(5, 4);"""
  , C.message C.assistant
      """That looks like a valid migration! Here's the migration you provided:

```sql
ALTER TABLE airlines_friendly_to_anthropologists
ALTER COLUMN discount_percentage TYPE DECIMAL(5, 4);
```
"""
  , C.message C.user "I need to add a new table to track the battles fought by the ancient Norse gods. The table should have the battle name, date of the battle, and the god involved. Can you help me create that?"
  , C.message C.assistant
      """Sure! Here’s the migration script to create a new table called norse_god_battles:

```sql
CREATE TABLE norse_god_battles (
    id SERIAL PRIMARY KEY,
    battle_name VARCHAR(255) NOT NULL,
    battle_date DATE NOT NULL,
    god_id INTEGER REFERENCES favorite_ancient_norse_gods(id) ON DELETE CASCADE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

- `id`: A unique identifier for each battle (primary key).
- `battle_name`: The name of the battle (required).
- `battle_date`: The date on which the battle took place (required).
- `god_id`: A foreign key linking to the `favorite_ancient_norse_gods` table, representing the god involved in the battle.
- `created_at`: A timestamp for when the record was created.

This table will help you track battles fought by the ancient Norse gods.
"""
  , C.message C.user "I need to update the anthropologists_researching_ancient_norse_gods table with new information. Can you help me with that?"
  , C.message C.assistant
      """I'd be happy to help! Could you please clarify what kind of update you need for the anthropologists_researching_ancient_norse_gods table? Are you looking to:

1. Add a new column?
2. Modify an existing column?
3. Insert new data into the table?
4. Update existing data?
5. Change the structure of the table in some other way?

Once I have more details, I can provide the appropriate migration or SQL statement for your needs.
"""
  , C.message C.user "Sorry, I need to add a birthday column."
  , C.message C.assistant
      """To add a `birthday` column to the `anthropologists_researching_ancient_norse_gods` table, you can use the following migration script:
```sql
ALTER TABLE anthropologists_researching_ancient_norse_gods
ADD COLUMN birthday DATE;
```

This script adds a new birthday column of type DATE to the anthropologists_researching_ancient_norse_gods table. After running this migration, you'll be able to store the birth dates of the anthropologists in this table.
"""
  , C.message C.user "Can you help me establish a foreign key relationship between obscure_norse_god_research_papers and favorite_ancient_norse_gods?"
  , C.message C.assistant
      """Certainly! Here’s the migration script to establish a foreign key relationship between the `obscure_norse_god_research_papers` and `favorite_ancient_norse_gods` tables:
    
```sql
ALTER TABLE obscure_norse_god_research_papers
ADD COLUMN favorite_god_id INTEGER;

ALTER TABLE obscure_norse_god_research_papers
ADD CONSTRAINT fk_obscure_norse_god_research_papers_favorite_god
FOREIGN KEY (favorite_god_id)
REFERENCES favorite_ancient_norse_gods(id)
ON DELETE SET NULL;
```
"""
  , C.message C.user
      """--raw
ALTER airlines_friendly_to_anthropologists
CHANGE COLUMN airline_name TEXT;
"""
  , C.message C.assistant
      """
It looks like there’s a small issue with the syntax. In PostgreSQL, you need to use the ALTER TABLE statement along with the ALTER COLUMN clause to change a column's type. Here’s the corrected migration script:

```sql
ALTER TABLE airlines_friendly_to_anthropologists
ALTER COLUMN airline_name TYPE TEXT;
```

This script will correctly change the `airline_name` column to `TEXT`.
"""
  ]

user :: Ask -> String
user (Ask ask) = ask
