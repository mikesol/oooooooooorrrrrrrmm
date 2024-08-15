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
import OOOOOOOOOORRRRRRRMM.Bootstrap (bootstrap)
import OOOOOOOOOORRRRRRRMM.BootstrapTmp (bootstrapTmp)
import OOOOOOOOOORRRRRRRMM.Migrate (migrate)
import OOOOOOOOOORRRRRRRMM.PreCommit (preCommit)
import OOOOOOOOOORRRRRRRMM.PureScript (pureScript)
import OOOOOOOOOORRRRRRRMM.Query (query)
import OOOOOOOOOORRRRRRRMM.Question (question)
import OOOOOOOOOORRRRRRRMM.Schema (schema)
import OOOOOOOOOORRRRRRRMM.TypeScript (typescript)

main :: Effect Unit
main = do
  args <- argv
  case parseArgs "my-cli" "This is my CLI." parser (Array.drop 2 args) of
    Left err -> log $ printArgError err
    Right success -> case success of
      Migrate m -> launchAff_ do migrate m
      Query q -> launchAff_ do query q
      PureScript p -> launchAff_ do pureScript p
      Typescript t -> launchAff_ do typescript t
      Schema s -> launchAff_ do schema s
      Question s -> launchAff_ do question s
      BootstrapTmp b -> launchAff_ do bootstrapTmp b
      Bootstrap b -> launchAff_ do bootstrap b
      PreCommit pc -> launchAff_ do preCommit pc
