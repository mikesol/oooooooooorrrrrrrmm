module Test.Query.UserWithAGivenEmailAddress where
import Data.Nullable (Nullable)

type I = { "$1" :: String }

type Q = """
SELECT * FROM users WHERE email = $1;
"""

type O = Array { id :: Int, email :: Nullable String, first_name :: String, last_name :: String, username :: String, verified :: Boolean, phone_number :: Nullable String, image_url :: Nullable String }