module OOOOOOOOOORRRRRRRMM.Prompts.DoBinding where

import Prelude

import OOOOOOOOOORRRRRRRMM.Completions as C
import OOOOOOOOOORRRRRRRMM.Prompts.NorseGods (norseGods)

newtype Schema = Schema String
newtype Query = Query String

system :: Schema -> String
system (Schema schema) =
  """You are a helpful assistant that generates JSON describing Postgres SQL queries. The JSON must conform to this schema:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "description": "A schema representing the structure of input parameters and output columns for a PostgreSQL query.",
  "properties": {
    "input": {
      "type": "object",
      "description": "Object representing the input parameters of the PostgreSQL query.",
      "patternProperties": {
        "^[a-zA-Z_][a-zA-Z0-9_]*$": {
          "type": "object",
          "description": "Details about a specific input parameter.",
          "properties": {
            "type": {
              "type": "string",
              "description": "The PostgreSQL data type of the input parameter.",
              "enum": [
                "integer",
                "boolean",
                "timestamp",
                "timestampz",
                "json",
                "text",
                "varchar",
                "numeric",
                "uuid",
                "bytea",
                "date",
                "time",
                "timetz",
                "interval",
                "record",
                "enum",
                "tsvector",
                "tsquery"
              ]
            },
            "is_array": {
              "type": "boolean",
              "description": "Indicates if the input parameter is an array of the specified type.",
              "default": false
            },
            "is_nullable": {
              "type": "boolean",
              "description": "Indicates if the input parameter can be null.",
              "default": false
            },
            "position_starting_from_1": {
              "type": "integer",
              "description": "The 1-based position of this parameter in the SQL query (e.g., $1, $2). Note that literals should _not_ be part of this count.",
              "minimum": 1
            }
          },
          "required": ["type", "is_array", "is_nullable", "position_starting_from_1"],
          "description": "Defines a single input parameter with its type, whether it's an array, and its positional index in the query."
        }
      },
      "additionalProperties": false,
      "description": "Container for all input parameters of the query, keyed by parameter names."
    },
    "output": {
      "type": "object",
      "description": "Object representing the output columns of the PostgreSQL query.",
      "patternProperties": {
        "^[a-zA-Z_][a-zA-Z0-9_]*$": {
          "type": "object",
          "description": "Details about a specific output column.",
          "properties": {
            "type": {
              "type": "string",
              "description": "The PostgreSQL data type of the output column.",
              "enum": [
                "integer",
                "boolean",
                "timestamp",
                "timestampz",
                "json",
                "text",
                "varchar",
                "numeric",
                "uuid",
                "bytea",
                "date",
                "time",
                "timetz",
                "interval",
                "record",
                "enum",
                "tsvector",
                "tsquery"
              ]
            },
            "is_array": {
              "type": "boolean",
              "description": "Indicates if the output column is an array of the specified type.",
              "default": false
            },
            "is_nullable": {
              "type": "boolean",
              "description": "Indicates if the output column can be null.",
              "default": false
            }
          },
          "required": ["type", "is_nullable", "is_array"],
          "description": "Defines a single output column with its type and whether it's an array."
        }
      },
      "additionalProperties": false,
      "description": "Container for all output columns of the query, keyed by column names."
    }
  },
  "required": ["input", "output"],
  "additionalProperties": false,
  "description": "A JSON Schema that defines the structure of inputs and outputs for a PostgreSQL query, including the data types, positional parameters for inputs, and array indicators."
}
```

Additionally, so that you know the types of the whole postgres db, here is a db dump from `pg_dump`:

```sql
""" <> schema <> norseGods <>
    """
```
"""

loremIpsum :: Array C.Message
loremIpsum =
  [ C.message C.user $ user $ Query
      """SELECT name, domain
FROM favorite_ancient_norse_gods;"""
  , C.message C.assistant $ assistant $ Json
      """{
  "input": {},
  "output": {
    "name": {
      "type": "varchar",
      "is_nullable": false,
      "is_array": false
    },
    "domain": {
      "type": "varchar",
      "is_nullable": true,
      "is_array": false
    }
  }
}"""
  , C.message C.user $ user $ Query
      """SELECT title, publication_year
FROM obscure_norse_god_research_papers
WHERE publication_year = $1;"""
  , C.message C.assistant $ assistant $ Json
      """{
  "input": {
    "publication_year": {
      "type": "integer",
      "is_array": false,
      "is_nullable": false,
      "position_starting_from_1": 1
    }
  },
  "output": {
    "title": {
      "type": "varchar",
      "is_nullable": false,
      "is_array": false
    },
    "publication_year": {
      "type": "integer",
      "is_nullable": false,
      "is_array": false
    }
  }
}
"""
  , C.message C.user $ user $ Query
      """SELECT a.full_name, g.name AS favorite_god_name
FROM anthropologists_researching_ancient_norse_gods a
JOIN favorite_ancient_norse_gods g ON a.favorite_god_id = g.id;"""
  , C.message C.assistant $ assistant $ Json
      """{
  "input": {},
  "output": {
    "full_name": {
      "type": "varchar",
      "is_nullable": false,
      "is_array": false
    },
    "favorite_god_name": {
      "type": "varchar",
      "is_nullable": false,
      "is_array": false
    }
  }
}
"""
  , C.message C.user $ user $ Query
      """SELECT g.name, COUNT(b.id) AS battles_count
FROM favorite_ancient_norse_gods g
JOIN norse_god_battles b ON g.id = b.god_id
GROUP BY g.name;"""
  , C.message C.assistant $ assistant $ Json
      """{
  "input": {},
  "output": {
    "name": {
      "type": "varchar",
      "is_nullable": false,
      "is_array": false
    },
    "battles_count": {
      "type": "integer",
      "is_nullable": false,
      "is_array": false
    }
  }
}
"""
  , C.message C.user $ user $ Query
      """INSERT INTO favorite_ancient_norse_gods (name, domain, worshipers_count)
VALUES ('Loki', $2, $1);
"""
  , C.message C.assistant $ assistant $ Json
      """{
  "input": {
    "worshipers_count": {
      "type": "integer",
      "is_array": false,
      "is_nullable": true,
      "position_starting_from_1": 1
    },
    "domain": {
      "type": "varchar",
      "is_array": false,
      "is_nullable": true,
      "position_starting_from_1": 2
    }
  },
  "output": {}
}"""
  , C.message C.user $ user $ Query
      """SELECT *
FROM obscure_norse_god_research_papers p
JOIN anthropologists_researching_ancient_norse_gods a ON p.anthropologist_id = a.id
JOIN favorite_ancient_norse_gods g ON a.favorite_god_id = g.id
WHERE a.full_name = 'Jane Doe' AND g.name = 'Odin';

"""
  , C.message C.assistant $ assistant $ Json
      """{
  "input": {
    "full_name": {
      "type": "varchar",
      "is_array": false,
      "is_nullable": false,
      "position_starting_from_1": 1
    },
    "god_name": {
      "type": "varchar",
      "is_array": false,
      "is_nullable": false,
      "position_starting_from_1": 2
    }
  },
  "output": {
    "id": {
      "type": "integer",
      "is_nullable": false,
      "is_array": false
    },
    "title": {
      "type": "varchar",
      "is_nullable": false,
      "is_array": false
    },
    "publication_year": {
      "type": "integer",
      "is_nullable": false,
      "is_array": false
    },
    "anthropologist_id": {
      "type": "integer",
      "is_nullable": false,
      "is_array": false
    },
    "created_at": {
      "type": "timestamp",
      "is_nullable": false,
      "is_array": false
    },
    "full_name": {
      "type": "varchar",
      "is_nullable": false,
      "is_array": false
    },
    "specialty": {
      "type": "varchar",
      "is_nullable": true,
      "is_array": false
    },
    "favorite_god_id": {
      "type": "integer",
      "is_nullable": true,
      "is_array": false
    },
    "research_start_date": {
      "type": "date",
      "is_nullable": true,
      "is_array": false
    },
    "god_name": {
      "type": "varchar",
      "is_nullable": false,
      "is_array": false
    },
    "domain": {
      "type": "varchar",
      "is_nullable": true,
      "is_array": false
    },
    "worshipers_count": {
      "type": "integer",
      "is_nullable": true,
      "is_array": false
    }
  }
}
"""
  ]

newtype Json = Json String

assistant :: Json -> String
assistant (Json json) =
  """Here is the JSON analysis of the inputs and outputs of the query you provided:

```json
""" <> json <>
    """
```
"""

user :: Query -> String
user (Query query) =
  """Could you please send me back a JSON analysis of the inputs into (if any) and outputs out of (if any) the following query?

```sql
""" <> query <>
    """
```
"""
