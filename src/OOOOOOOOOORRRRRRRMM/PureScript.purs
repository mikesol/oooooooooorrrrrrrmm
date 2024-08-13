module OOOOOOOOOORRRRRRRMM.PureScript where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (fold, (!!), (..))
import Data.Array as Array
import Data.CodePoint.Unicode (isAlphaNum, isUpper)
import Data.Either (Either(..))
import Data.Filterable (filter, filterMap)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (over)
import Data.String (codePointFromChar)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Extra (kebabCase, pascalCase)
import Effect.Aff (Aff, error, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Node.Buffer (toString)
import Node.ChildProcess (exec')
import Node.Encoding (Encoding(..))
import Node.Encoding as Encoding
import Node.FS.Aff (mkdir, readTextFile, readdir, writeTextFile)
import Node.FS.Sync (exists)
import Node.Path (FilePath)
import Node.Path as Path
import Node.ReadLine (close, createConsoleInterface, noCompletion)
import OOOOOOOOOORRRRRRRMM.Arrrrrgs (PureScript)
import OOOOOOOOOORRRRRRRMM.OpenAI (ChatCompletionRequest(..), ChatCompletionResponse(..), ResponseFormat(..), ccr, createCompletions, message, system, user)
import OOOOOOOOOORRRRRRRMM.Pg (Database(..), Host(..), Port(..), User(..), closeClient, newClient, parsePostgresUrl, runSqlCommand)
import OOOOOOOOOORRRRRRRMM.Prompts.DoPurescript as DoPurescript
import Safe.Coerce (coerce)
import Yoga.JSON (class ReadForeign, readJSON_, writeJSON)

newtype QueryResult = QueryResult { result :: String, success :: Boolean }

derive newtype instance ReadForeign QueryResult

newtype FixQueryResult = FixQueryResult { result :: String, success :: Boolean, revised :: Boolean }

derive newtype instance ReadForeign FixQueryResult

startInstanceCmd :: String
startInstanceCmd = "pg_tmp -t"

pgDumpCmd :: Database -> Host -> Port -> User -> FilePath -> String
pgDumpCmd (Database database) (Host host) (Port port) (User user) fp =
  "pg_dump -d "
    <> database
    <> " -h "
    <> host
    <> " -p "
    <> writeJSON port
    <> " -U "
    <> user
    <> " > "
    <> fp

migrationsStartAt0AndIncreaseBy1 :: Array Int -> Boolean
migrationsStartAt0AndIncreaseBy1 = go 0
  where
  go :: Int -> Array Int -> Boolean
  go i a = case Array.uncons a of
    Nothing -> true
    Just { head, tail } -> head == i && go (i + 1) tail

pureScript :: PureScript -> Aff Unit
pureScript info = do
  let splitPrefix = String.split (String.Pattern ".") info.prefix
  let
    prefixIsValid = append (Conj (Array.length splitPrefix > 0))
      $ fold
      $ map
          ( toCharArray
              >>> map codePointFromChar
              >>> \a -> Conj (Array.length a > 0) <> Conj ((isUpper <$> (a !! 0)) == Just true)
                <> fold (map (isAlphaNum >>> Conj) $ Array.drop 1 a)
          )
          splitPrefix
  when (not $ coerce prefixIsValid) do
    throwError $ error "The prefix must be a valid purescript module name."
  pthExists <- liftEffect $ exists info.ps
  when (not pthExists) do
    void $ mkdir info.ps
  for_ (1 .. Array.length splitPrefix) \ix -> do
    let xpth = Path.concat $ [ info.ps ] <> Array.take ix splitPrefix
    xpthExists <- liftEffect $ exists xpth
    when (not xpthExists) do
      void $ mkdir xpth
  console <- liftEffect $ createConsoleInterface noCompletion
  migrationPaths <- readdir info.migrations
  let migrations = filterMap readJSON_ migrationPaths
  let migrationsAreSane = migrationsStartAt0AndIncreaseBy1 migrations
  when (not migrationsAreSane) do
    throwError $ error "Migrations must start at 0 and increase by 1."
  let meta = Path.concat [ info.queries, "__meta" ]
  metaExists <- liftEffect $ exists meta
  when (not metaExists) do
    void $ mkdir meta
  queryPaths <- filter (eq <*> kebabCase) <$> readdir info.queries
  let rawQ = Path.concat [ info.queries, "__raw" ]
  rawQExists <- liftEffect $ exists rawQ
  when (not rawQExists) do
    void $ mkdir rawQ
  rawQPaths <- readdir rawQ
  let rawM = Path.concat [ info.migrations, "__raw" ]
  rawMExists <- liftEffect $ exists rawM
  rawMigrations <- if not rawMExists then pure [] else readdir rawM
  log "Starting postgres 🤓"
  url <- makeAff \f -> do
    void $ exec' startInstanceCmd identity \{ error: e, stdout } -> case e of
      Nothing -> do
        url <- toString UTF8 stdout
        f $ Right url
      Just _ -> f $ Left $ error "Failed to start postgres."
    mempty
  parsed <- case parsePostgresUrl url of
    Nothing -> throwError $ error "Could not parse the postgres url."
    Just x -> pure x
  client <- newClient parsed.host parsed.port parsed.user parsed.database
  for_ rawMigrations \migrationIx -> do
    let migrationPath = Path.concat [ rawM, migrationIx ]
    log $ "Setting up ephemeral db with migration in " <> migrationPath
    sql <- readTextFile Encoding.UTF8 migrationPath
    void $ runSqlCommand client sql mempty
  let
    getSchema = do
      rn <- liftEffect $ randomInt 0 42424242
      let schemaPath = Path.concat [ "/tmp", "schema-" <> writeJSON rn <> ".sql" ]
      makeAff \f -> do
        void $ exec' (pgDumpCmd parsed.database parsed.host parsed.port parsed.user schemaPath) identity $ \{ error: e } -> do
          case e of
            Nothing -> f $ Right unit
            Just _ -> f $ Left $ error $ "Could not dump the pg database during the migration 😔 " <> show e
        mempty
      readTextFile Encoding.UTF8 schemaPath
  let
    go :: Array String -> Aff (Step (Array String) Unit)
    go queryArr = do
      schema <- getSchema
      case Array.head queryArr of
        Nothing -> pure $ Done unit
        Just q -> do
          let queryPath = Path.concat [ info.queries, "__raw", q ]
          log $ "Reading query from " <> queryPath
          queryText <- readTextFile Encoding.UTF8 queryPath
          let systemM = DoPurescript.system
          let moduleFileName = pascalCase q
          let moduleName = info.prefix <> "." <> pascalCase q
          let userM = DoPurescript.user (DoPurescript.Schema schema) (DoPurescript.Query queryText) (DoPurescript.ModuleName moduleName)
          ChatCompletionResponse { choices } <- createCompletions
            $ over ChatCompletionRequest
                _
                  { messages =
                      [ message system systemM
                      , message user userM
                      ]
                  , response_format = pure $ ResponseFormat DoPurescript.responseFormat
                  }
                ccr
          QueryResult { result, success } <- maybe (throwError $ error "No migration could be generated") pure do
            { message: { content } } <- choices !! 0
            content >>= readJSON_
          if not success then do
            log result
            pure $ Done unit
          else do
            log "Creating PureScript file"
            -- now it's safe to write the query
            let newModulePath = Path.concat ([ info.ps ] <> splitPrefix <> [ moduleFileName <> ".purs" ])
            writeTextFile Encoding.UTF8 newModulePath result
            pure $ Loop $ Array.drop 1 queryArr

  tailRecM go (filter (flip Array.elem queryPaths) rawQPaths)
  closeClient client
  liftEffect $ close console
