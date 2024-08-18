module OOOOOOOOOORRRRRRRMM.Arrrrrgs where

import Prelude

import ArgParse.Basic (ArgParser, argument, boolean, choose, command, default, flag, flagHelp, fromRecord, int, optional)
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.Show.Generic (genericShow)
import Yoga.JSON (class ReadForeign)

newtype RCFile = RCFile { url :: Maybe String, model :: Maybe String, token :: Maybe String }

derive instance Newtype RCFile _
derive newtype instance ReadForeign RCFile
derive newtype instance Semigroup RCFile
derive newtype instance Monoid RCFile

data Validator = Zod | IoTs

derive instance Generic Validator _

instance Show Validator where
  show = genericShow

type Migrate =
  { migrations :: String
  , queries :: String
  , context :: String
  , schema :: String
  , yes :: Boolean
  , tries :: Int
  , url :: String
  , model :: String
  , token :: Maybe String
  }

type PreCommit =
  { migrations :: String
  , queries :: String
  }

type Query =
  { queries :: String
  , migrations :: String
  , yes :: Boolean
  , url :: String
  , model :: String
  , token :: Maybe String
  }

type PureScript =
  { queries :: String
  , migrations :: String
  , ps :: String
  , prefix :: String
  , url :: String
  , model :: String
  , token :: Maybe String
  }

type Typescript =
  { queries :: String
  , migrations :: String
  , ts :: String
  , validator :: Maybe Validator
  , url :: String
  , model :: String
  , token :: Maybe String
  }

type Schema =
  { migrations :: String
  , path :: String
  , humanReadable :: Boolean
  , url :: String
  , model :: String
  , token :: Maybe String
  }

type Question =
  { migrations :: String
  , question :: String
  , url :: String
  , model :: String
  , token :: Maybe String
  }

type BootstrapTmp = { args :: String, migrations :: String }
type Bootstrap = { connectionString :: String, migrations :: String, startingMigration :: Int }

data Arrrrrgs
  = Migrate Migrate
  | Query Query
  | PureScript PureScript
  | Typescript Typescript
  | Schema Schema
  | Question Question
  | BootstrapTmp BootstrapTmp
  | Bootstrap Bootstrap
  | PreCommit PreCommit

derive instance Generic Arrrrrgs _

instance Show Arrrrrgs where
  show = genericShow

migrate ∷ RCFile -> ArgParser Migrate
migrate rc = command [ "migrate", "m" ] "Create migrations." do
  flagHelp *> fromRecord
    { migrations:
        argument
          [ "--migrations", "-m" ]
          "The directory with the migrations. Must be sequential, starting from 0. The first one without a corresponding record in the db will be run."
          # default "migrations"
    , queries:
        argument
          [ "--queries", "-q" ]
          "The directory with the queries. All queries must be kebab case. Anything that's not kebab case won't be considered a query and will be ignored."
          # default "queries"
    , schema:
        argument
          [ "--schema", "-s" ]
          "The directory where the new schema will be written."
          # default "schema"
    , context:
        argument
          [ "--context", "-c" ]
          "Context needed for the migrations. In the form <query>/<index>, where <query> is the name of the query and <index> is the index of the migration that needs context."
          # default "context"
    , yes:
        flag
          [ "--yes", "-y" ]
          "Bypasses review and just writes the migration." # boolean
    , url:
        argument
          [ "--url", "-u" ]
          "The url to run the query against."
          # default (fromMaybe "https://api.openai.com/v1/chat/completions" (un RCFile rc).url)
    , model:
        argument
          [ "--model", "-ml" ]
          "The model to use."
          # default (fromMaybe "gpt-4o" (un RCFile rc).url)
    , token:
        argument
          [ "--token", "-t" ]
          "The bearer token for authentication"
          # optional
          <#> (_ <|> (un RCFile rc).token)
    , tries:
        argument
          [ "--tries", "-t" ]
          "How many times to try the migration on failure." # int # default 3
    }

preCommit ∷ ArgParser PreCommit
preCommit = command [ "pre-commit", "pc" ] "A pre-commit hook" do
  flagHelp *> fromRecord
    { migrations:
        argument
          [ "--migrations", "-m" ]
          "The directory with the migrations. Must be sequential, starting from 0. The first one without a corresponding record in the db will be run."
          # default "migrations"
    , queries:
        argument
          [ "--queries", "-q" ]
          "The directory with the queries. All queries must be kebab case. Anything that's not kebab case won't be considered a query and will be ignored."
          # default "queries"
    }

query ∷ RCFile -> ArgParser Query
query rc = command [ "query", "q" ] "Create queries." do
  flagHelp *> fromRecord
    { queries:
        argument
          [ "--queries", "-q" ]
          "The directory with the queries."
          # default "queries"
    , migrations:
        argument
          [ "--migrations", "-m" ]
          "The directory with the migrations. Must be sequential, starting from 0. The first one without a corresponding record in the db will be run."
          # default "migrations"
    , url:
        argument
          [ "--url", "-u" ]
          "The url to run the query against."
          # default (fromMaybe "https://api.openai.com/v1/chat/completions" (un RCFile rc).url)
    , model:
        argument
          [ "--model", "-ml" ]
          "The model to use."
          # default (fromMaybe "gpt-4o" (un RCFile rc).url)
    , token:
        argument
          [ "--token", "-t" ]
          "The bearer token for authentication"
          # optional
          <#> (_ <|> (un RCFile rc).token)
    , yes:
        flag
          [ "--yes", "-y" ]
          "Bypasses review and just writes the query." # boolean
    }

pureScript ∷ RCFile -> ArgParser PureScript
pureScript rc = command [ "purescript", "ps" ] "Create purescript bindings." do
  flagHelp *> fromRecord
    { queries:
        argument
          [ "--queries", "-q" ]
          "The directory with the queries."
          # default "queries"
    , migrations:
        argument
          [ "--migrations", "-m" ]
          "The directory with the migrations. Must be sequential, starting from 0. The first one without a corresponding record in the db will be run."
          # default "migrations"
    , url:
        argument
          [ "--url", "-u" ]
          "The url to run the query against."
          # default (fromMaybe "https://api.openai.com/v1/chat/completions" (un RCFile rc).url)
    , model:
        argument
          [ "--model", "-ml" ]
          "The model to use."
          # default (fromMaybe "gpt-4o" (un RCFile rc).url)
    , token:
        argument
          [ "--token", "-t" ]
          "The bearer token for authentication"
          # optional
          <#> (_ <|> (un RCFile rc).token)
    , prefix:
        argument
          [ "--prefix", "-x" ]
          "The prefix for the generated PureScript modules. For example, MyApp.MyQueries."
          # default "OOOOOOOOOORRRRRRRMM.Wuz.Here"
    , ps:
        argument
          [ "--path", "-p" ]
          "The path to your PureScript code. Usually this is src, but in monorepos it's likely something a bit more elaborate."
          # default "src"
    }

typescript ∷ RCFile -> ArgParser Typescript
typescript rc = command [ "typescript", "ts" ] "Create typescript bindings." do
  flagHelp *> fromRecord
    { queries:
        argument
          [ "--queries", "-q" ]
          "The directory with the queries."
          # default "queries"
    , migrations:
        argument
          [ "--migrations", "-m" ]
          "The directory with the migrations. Must be sequential, starting from 0. The first one without a corresponding record in the db will be run."
          # default "migrations"
    , url:
        argument
          [ "--url", "-u" ]
          "The url to run the query against."
          # default (fromMaybe "https://api.openai.com/v1/chat/completions" (un RCFile rc).url)
    , model:
        argument
          [ "--model", "-ml" ]
          "The model to use."
          # default (fromMaybe "gpt-4o" (un RCFile rc).url)
    , token:
        argument
          [ "--token", "-t" ]
          "The bearer token for authentication"
          # optional
          <#> (_ <|> (un RCFile rc).token)
    , ts:
        argument
          [ "--path", "-p" ]
          "The path to your Typescript queries, ie src/queries"
          # default "src/queries"
    , validator:
        argument
          [ "--validator", "-v" ]
          "The validator to use. Valid values are `zod`` and `io-ts`."
          # optional
          <#> case _ of
            Just "zod" -> Just Zod
            Just "io-ts" -> Just IoTs
            _ -> Nothing

    }

schema ∷ RCFile -> ArgParser Schema
schema rc = command [ "schema", "s" ] "Export the schema." do
  flagHelp *> fromRecord
    { path:
        argument
          [ "--path", "-p" ]
          "The file to export the schema to."
          # default "schema.sql"
    , migrations:
        argument
          [ "--migrations", "-m" ]
          "The directory with the migrations. Must be sequential, starting from 0. The first one without a corresponding record in the db will be run."
          # default "migrations"
    , humanReadable:
        flag
          [ "--legible", "-l" ]
          "Make the schema human readable, getting rid of Postgres gunk."
          # boolean
    , url:
        argument
          [ "--url", "-u" ]
          "The url to run the query against."
          # default (fromMaybe "https://api.openai.com/v1/chat/completions" (un RCFile rc).url)
    , model:
        argument
          [ "--model", "-ml" ]
          "The model to use."
          # default (fromMaybe "gpt-4o" (un RCFile rc).url)
    , token:
        argument
          [ "--token", "-t" ]
          "The bearer token for authentication"
          # optional
          <#> (_ <|> (un RCFile rc).token)
    }

question ∷ RCFile -> ArgParser Question
question rc = command [ "ask", "a" ] "Ask a question." do
  flagHelp *> fromRecord
    { question:
        argument
          [ "--question", "-q" ]
          "The question to ask."
    , migrations:
        argument
          [ "--migrations", "-m" ]
          "The directory with the migrations. Must be sequential, starting from 0. The first one without a corresponding record in the db will be run."
          # default "migrations"
    , url:
        argument
          [ "--url", "-u" ]
          "The url to run the query against."
          # default (fromMaybe "https://api.openai.com/v1/chat/completions" (un RCFile rc).url)
    , model:
        argument
          [ "--model", "-ml" ]
          "The model to use."
          # default (fromMaybe "gpt-4o" (un RCFile rc).url)
    , token:
        argument
          [ "--token", "-t" ]
          "The bearer token for authentication"
          # optional
          <#> (_ <|> (un RCFile rc).token)
    }

bootstrapTmp ∷ ArgParser BootstrapTmp
bootstrapTmp = command [ "bootstrap-tmp", "bt" ] "Bootstraps a temporary db with your migrations, printing the url to stdout." do
  flagHelp *> fromRecord
    { args:
        argument
          [ "--args", "-a" ]
          "The arguments to pass to pg_tmp." # default " -t"
    , migrations:
        argument
          [ "--migrations", "-m" ]
          "The directory with the migrations. Must be sequential, starting from 0. The first one without a corresponding record in the db will be run."
          # default "migrations"
    }

bootstrap ∷ ArgParser Bootstrap
bootstrap = command [ "bootstrap", "b" ] "Bootstraps a db with your migrations." do
  flagHelp *> fromRecord
    { connectionString:
        argument
          [ "--connection-string", "-c" ]
          "The connection string for your db."
    , migrations:
        argument
          [ "--migrations", "-m" ]
          "The directory with the migrations. Must be sequential, starting from 0. The first one without a corresponding record in the db will be run."
          # default "migrations"
    , startingMigration:
        argument [ "--starting-migration", "-sm" ]
          "The migration to start from" # int # default 0
    }

parser :: RCFile -> ArgParser Arrrrrgs
parser rc = choose
  "command"
  [ Migrate <$> migrate rc
  , Query <$> query rc
  , PureScript <$> pureScript rc
  , Typescript <$> typescript rc
  , Schema <$> schema rc
  , Question <$> question rc
  , BootstrapTmp <$> bootstrapTmp
  , Bootstrap <$> bootstrap
  , PreCommit <$> preCommit
  ]
