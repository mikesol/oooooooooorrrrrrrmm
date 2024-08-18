module Main where

import Prelude

import ArgParse.Basic (parseArgs, printArgError)
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign.Object (Object, lookup)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile)
import Node.Process (argv, getEnv)
import OOOOOOOOOORRRRRRRMM.Arrrrrgs (Arrrrrgs(..), RCFile(..), parser)
import OOOOOOOOOORRRRRRRMM.Bootstrap (bootstrap)
import OOOOOOOOOORRRRRRRMM.BootstrapTmp (bootstrapTmp)
import OOOOOOOOOORRRRRRRMM.Migrate (migrate)
import OOOOOOOOOORRRRRRRMM.PreCommit (preCommit)
import OOOOOOOOOORRRRRRRMM.PureScript (pureScript)
import OOOOOOOOOORRRRRRRMM.Query (query)
import OOOOOOOOOORRRRRRRMM.Question (question)
import OOOOOOOOOORRRRRRRMM.Schema (schema)
import OOOOOOOOOORRRRRRRMM.TypeScript (typescript)
import Yoga.JSON (readJSON_)

configFile = "oooooooooorrrrrrrmm.config.json" :: String

chooseEnv :: Object String -> RCFile -> RCFile
chooseEnv env (RCFile { token, url, model }) = RCFile
  { token: lookup "COMPLETIONS_TOKEN" env <|> token
  , url: lookup "COMPLETIONS_URL" env <|> url
  , model: lookup "COMPLETIONS_MODEL" env <|> model
  }

main :: Effect Unit
main = do
  rcExists <- exists configFile
  rc <- chooseEnv <$> getEnv <*>
    if not rcExists then pure mempty
    else do
      log $ "Found " <> configFile <> ". Running commands from it."
      rc <- readJSON_ <$> readTextFile UTF8 configFile
      case rc of
        Nothing -> pure $ mempty
        Just (RCFile rc') -> pure $ RCFile rc'
  args <- argv
  case parseArgs "my-cli" "This is my CLI." (parser rc) (Array.drop 2 args) of
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
