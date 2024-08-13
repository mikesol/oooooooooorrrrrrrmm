module OOOOOOOOOORRRRRRRMM.Arrrrrgs where

import Prelude

import ArgParse.Basic (ArgParser, argument, choose, command, default, flagHelp, fromRecord)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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
  }

data Arrrrrgs
  = Migrate Migrate
  | Query Query
  | PureScript PureScript
  | Typescript Typescript

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
    }

parser :: ArgParser Arrrrrgs
parser = choose
  "command"
  [ Migrate <$> migrate, Query <$> query, PureScript <$> pureScript, Typescript <$> typescript ]
