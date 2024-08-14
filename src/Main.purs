module Main where

import Prelude

import ArgParse.Basic (parseArgs, printArgError)
import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Process (argv)
import OOOOOOOOOORRRRRRRMM.Arrrrrgs (Arrrrrgs(..), parser)
import OOOOOOOOOORRRRRRRMM.Migrate (migrate)
import OOOOOOOOOORRRRRRRMM.PureScript (pureScript)
import OOOOOOOOOORRRRRRRMM.Query (query)
import OOOOOOOOOORRRRRRRMM.Schema (schema)
import OOOOOOOOOORRRRRRRMM.TypeScript (typescript)
import OOOOOOOOOORRRRRRRMM.Question (question)
main :: Effect Unit
main = do
  args <- argv
  case parseArgs "my-cli" "This is my CLI." parser (Array.drop 2 args) of
    Left err -> log $ printArgError err
    Right (Migrate m) -> launchAff_ do migrate m
    Right (Query q) -> launchAff_ do query q
    Right (PureScript p) -> launchAff_ do pureScript p
    Right (Typescript t) -> launchAff_ do typescript t
    Right (Schema s) -> launchAff_ do schema s
    Right (Question s) -> launchAff_ do question s
