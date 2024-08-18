module OOOOOOOOOORRRRRRRMM.Query where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (compact, filter, filterMap)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over)
import Data.String.Extra (kebabCase)
import Data.Traversable (for)
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
import Node.ReadLine.Aff (question)
import OOOOOOOOOORRRRRRRMM.Arrrrrgs (Query)
import OOOOOOOOOORRRRRRRMM.Checksum (checksum)
import OOOOOOOOOORRRRRRRMM.Completions (ChatCompletionRequest(..), ChatCompletionResponse(..), ccr, createCompletions, message, system, user)
import OOOOOOOOOORRRRRRRMM.ConvertToResult (convertToResult)
import OOOOOOOOOORRRRRRRMM.Pg (Database(..), Host(..), Port(..), User(..), closeClient, newClient, parsePostgresUrl, runSqlCommand)
import OOOOOOOOOORRRRRRRMM.Prompts.DoQuery as DoQuery
import OOOOOOOOOORRRRRRRMM.Query.Metadata (Metadata(..))
import Yoga.JSON (class ReadForeign, readJSON_, writeJSON)

newtype QueryResult = QueryResult { result :: String, success :: Boolean }

derive newtype instance ReadForeign QueryResult

startInstanceCmd :: String
startInstanceCmd = "pg_tmp -t"

pgDumpCmd :: Database -> Host -> Port -> User -> FilePath -> String
pgDumpCmd (Database database) (Host host) (Port port) (User user) fp =
  "pg_dump --schema-only -d "
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

query :: Query -> Aff Unit
query info = do
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
  rawMigrations' <- if not rawMExists then pure [] else readdir rawM
  let (rawMigrations :: Array Int) = Array.sort $ compact $ map readJSON_ rawMigrations'
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
    let migrationPath = Path.concat [ rawM, writeJSON migrationIx ]
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
          let queryPath = Path.concat [ info.queries, q ]
          log $ "Reading query from " <> queryPath
          queryText <- readTextFile Encoding.UTF8 queryPath
          let systemM = DoQuery.system (DoQuery.Sql schema)
          let userM = DoQuery.user (DoQuery.Ask queryText)
          ChatCompletionResponse { choices } <- createCompletions info.url info.token
            $ over ChatCompletionRequest
                _
                  { model = info.model
                  , messages =
                      [ message system systemM
                      , message user userM
                      ]
                  }
                ccr
          { result, success } <- maybe (throwError $ error "No query could be generated") pure do
            { message: { content } } <- choices !! 0
            pure (convertToResult content)
          if not success then do
            log result
            pure $ Done unit
          else do
            let
              qtext =
                """Please review the query text inside the <query> tag below.

<query>
"""
                  <> result
                  <>
                    """
</query>
Press y or Y to accept and any other key to reject: """

            response <- if info.yes then pure "y" else question qtext console
            case response of
              x
                | x /= "y" && x /= "Y" -> do
                    log "Oh noes! Please change your prompt and try again."
                    pure $ Done unit
                | otherwise -> do
                    log $ if info.yes then "Creating query 🤔\n<query>\n" <> result <> "\n</query>\n" else "Great! Creating query 🤔"
                    -- now it's safe to write the query
                    let newQueryPath = Path.concat [ rawQ, q ]
                    let metaPath = Path.concat [ meta, q ]
                    writeTextFile Encoding.UTF8 newQueryPath result
                    metaPathExists <- liftEffect $ exists metaPath
                    let mostRecentMigrationPath = Path.concat [ rawM, writeJSON (Array.length migrations - 1) ]
                    mostRecentMigrationText <- readTextFile Encoding.UTF8 mostRecentMigrationPath
                    let
                      writeWithoutMerge = writeTextFile Encoding.UTF8 metaPath $ writeJSON
                        $ Metadata
                            { query_text_checksum: checksum queryText
                            , result_checksum: checksum result
                            , meta_version: 0
                            , purescript_binding: Nothing
                            , typescript_binding: Nothing
                            , most_recent_migration_checksum: checksum mostRecentMigrationText
                            }
                    if metaPathExists then do
                      rawPreviousMeta <- readTextFile Encoding.UTF8 metaPath
                      case readJSON_ rawPreviousMeta of
                        Just (Metadata { typescript_binding, purescript_binding }) -> writeTextFile Encoding.UTF8 metaPath $ writeJSON $ Metadata
                          { query_text_checksum: checksum queryText
                          , result_checksum: checksum result
                          , meta_version: 0
                          , purescript_binding
                          , typescript_binding
                          , most_recent_migration_checksum: checksum mostRecentMigrationText
                          }
                        Nothing -> writeWithoutMerge
                    else do writeWithoutMerge
                    pure $ Loop $ Array.drop 1 queryArr
  mostRecentMigration <- readTextFile Encoding.UTF8 $ Path.concat [ info.migrations, "__raw", writeJSON (Array.length migrations - 1) ]
  let mostRecentMigrationChecksum = checksum mostRecentMigration
  queriesToRun <- for queryPaths \qp -> do
    let runMe = pure $ Just qp
    if not (Array.elem qp rawQPaths) then runMe
    else do
      qt <- readTextFile Encoding.UTF8 $ Path.concat [ info.queries, qp ]
      let metaPath = Path.concat [ meta, qp ]
      let rawPath = Path.concat [ rawQ, qp ]
      pe <- liftEffect $ exists metaPath
      if not pe then runMe
      else do
        metadata <- readTextFile Encoding.UTF8 metaPath
        case readJSON_ metadata of
          Nothing -> runMe
          Just (Metadata { query_text_checksum, result_checksum, most_recent_migration_checksum }) -> do
            if mostRecentMigrationChecksum /= most_recent_migration_checksum then runMe
            else do
              rw <- liftEffect $ exists rawPath
              if not rw then runMe
              else if query_text_checksum /= checksum qt then runMe
              else do
                rt <- readTextFile Encoding.UTF8 rawPath
                if result_checksum /= checksum rt then runMe
                else pure $ Nothing
  tailRecM go $ compact queriesToRun
  closeClient client
  liftEffect $ close console
