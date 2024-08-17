module OOOOOOOOOORRRRRRRMM.Prompts.DoSchema where

import Prelude


newtype Schema = Schema String
system :: String
system = """You are a helpful assistant that takes schemas output by pg_dump and makes them human-readable."""

user :: Schema ->  String
user (Schema schema)   =
  """A postgres database was output by `pg_dump` with the following sql:
<sql>
""" <> schema
    <>
      """
</sql>

As you can see, there's some postgres gunk in there that comes just from the system. Here's an example in general of the type of thing I consider "gunk":

<sql>
SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;
</sql>

Some things to remove are:

- altering stuff to change the owner
- things like SELECT pg_catalog.setval

Basically, the only thing that should be left is the CREATE TABLE statements, the CREATE INDEX statements, and any triggers (if they look like they were made by a human). Also, please remove unnecessary namespacing, for example "public." if not needed.

Please send back the human-readable schema in backticks.
"""
