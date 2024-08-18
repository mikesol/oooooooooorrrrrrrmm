module OOOOOOOOOORRRRRRRMM.Prompts.DoSchema where

import Prelude

newtype Schema = Schema String

system :: String
system =
  """You are a helpful assistant that takes postgres schemas output by pg_dump and makes them human-readable, getting rid of things that seem like system gunk.

Some things to remove are:

- altering stuff to change the owner
- things like SELECT pg_catalog.setval

Basically, the only thing that should be left is the CREATE TABLE statements, the CREATE INDEX statements, and any triggers (if they look like they were made by a human). Also, please remove unnecessary namespacing, for example "public." if not needed.
"""

user :: Schema -> String
user (Schema schema) =
  """Could you please make this postgres schema a bit more human-readable?
  
```sql
""" <> schema
    <>
      """
```
"""
