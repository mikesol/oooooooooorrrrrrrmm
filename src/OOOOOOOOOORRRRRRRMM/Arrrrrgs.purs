module OOOOOOOOOORRRRRRRMM.Arrrrrgs where

import Prelude

import ArgParse.Basic (ArgParser, argument, boolean, choose, command, default, flag, flagHelp, fromRecord, int, optional)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data Validator = Zod | IoTs

derive instance Generic Validator _

instance Show Validator where
  show = genericShow

type Migrate =
  { migrations :: String
  , queries :: String
  , context :: String
  }

type Query =
  { queries :: String
  , migrations :: String
  }

type PureScript =
  { queries :: String
  , migrations :: String
  , ps :: String
  , prefix :: String
  }

type Typescript =
  { queries :: String
  , migrations :: String
  , ts :: String
  , validator :: Maybe Validator
  }

type Schema = { migrations :: String, path :: String, humanReadable :: Boolean }
type Question = { migrations :: String, question :: String }
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

derive instance Generic Arrrrrgs _

instance Show Arrrrrgs where
  show = genericShow

migrate ∷ ArgParser Migrate
migrate = command [ "migrate", "m" ] "Create migrations." do
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
    , context:
        argument
          [ "--context", "-c" ]
          "Context needed for the migrations. In the form <query>/<index>, where <query> is the name of the query and <index> is the index of the migration that needs context."
          # default "context"
    }

query ∷ ArgParser Query
query = command [ "query", "q" ] "Create queries." do
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
    }

pureScript ∷ ArgParser PureScript
pureScript = command [ "purescript", "ps" ] "Create purescript bindings." do
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

typescript ∷ ArgParser Typescript
typescript = command [ "typescript", "ts" ] "Create typescript bindings." do
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

schema ∷ ArgParser Schema
schema = command [ "schema", "s" ] "Export the schema." do
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
    }

question ∷ ArgParser Question
question = command [ "ask", "a" ] "Ask a question." do
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

parser :: ArgParser Arrrrrgs
parser = choose
  "command"
  [ Migrate <$> migrate
  , Query <$> query
  , PureScript <$> pureScript
  , Typescript <$> typescript
  , Schema <$> schema
  , Question <$> question
  , BootstrapTmp <$> bootstrapTmp
  , Bootstrap <$> bootstrap
  ]
