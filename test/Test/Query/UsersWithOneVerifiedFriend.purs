module Test.Query.UsersWithOneVerifiedFriend where

import Data.Nullable (Nullable)

type I = {} -- No input parameters as this query does not use any

type Q = """
SELECT DISTINCT u.* FROM users u JOIN friends f ON u.id = f.asker OR u.id = f.receiver;
"""

type O = Array { id :: Int, email :: Nullable String, first_name :: String, last_name :: String, username :: String, verified :: Boolean, phone_number :: Nullable String, image_url :: Nullable String }