module OOOOOOOOOORRRRRRRMM.Migrate
  ( FixQueryResult(..)
  , MigrationResult(..)
  , checksum
  , goQ
  , migrate
  , migrationsStartAt0AndIncreaseBy1
  , pgDumpCmd
  , startInstanceCmd
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (filter, filterMap)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over)
import Data.String as String
import Data.String.Extra (kebabCase)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, error, makeAff, throwError, try)
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
import OOOOOOOOOORRRRRRRMM.Arrrrrgs (Migrate)
import OOOOOOOOOORRRRRRRMM.OpenAI (ChatCompletionRequest(..), ChatCompletionResponse(..), ResponseFormat(..), ccr, createCompletions, message, system, user)
import OOOOOOOOOORRRRRRRMM.Pg (Database(..), Host(..), Port(..), User(..), closeClient, newClient, parsePostgresUrl, runSqlCommand)
import OOOOOOOOOORRRRRRRMM.Prompts.DoMigration as DoMigration
import OOOOOOOOOORRRRRRRMM.Prompts.FixQuery (AdditionalContext(..))
import OOOOOOOOOORRRRRRRMM.Prompts.FixQuery as FixQuery
import OOOOOOOOOORRRRRRRMM.Schema (schema)
import Yoga.JSON (class ReadForeign, readJSON_, writeJSON)

newtype MigrationResult = MigrationResult { result :: String, success :: Boolean }

derive newtype instance ReadForeign MigrationResult

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

goQ
  :: Int
  -> Migrate
  -> String
  -> String
  -> { done :: Array (Tuple String String)
     , todo :: Array String
     }
  -> Aff
       ( Step
           { done :: Array (Tuple String String)
           , todo :: Array String
           }
           (Either Unit (Array (Tuple String String)))
       )
goQ migrationIx info schema migrationResult { todo, done } = Array.head todo # maybe (pure $ Done $ Right done) \queryPath -> do
  log $ "Potentially revising query in " <> queryPath
  let rawQueryFile = Path.concat [ info.queries, "__raw", queryPath ]
  queryText <- readTextFile Encoding.UTF8 rawQueryFile
  let intentionFile = Path.concat [ info.queries, queryPath ]
  intentionText <- readTextFile Encoding.UTF8 intentionFile
  let contextPath = Path.concat [ info.context, queryPath, writeJSON migrationIx ]
  contextExists <- liftEffect $ exists contextPath
  context <-
    if contextExists then Just <<< AdditionalContext <$> readTextFile Encoding.UTF8 contextPath
    else pure Nothing
  let systemM = FixQuery.system
  let userM = FixQuery.user (FixQuery.Sql schema) (FixQuery.Migration migrationResult) (FixQuery.Intention intentionText) (FixQuery.Query queryText) context
  ChatCompletionResponse { choices } <- createCompletions
    $ over ChatCompletionRequest
        _
          { messages =
              [ message system systemM
              , message user userM
              ]
          , response_format = pure $ ResponseFormat FixQuery.responseFormat
          }
        ccr
  FixQueryResult { result, success, revised } <- maybe (throwError $ error "No fixed query could be generated") pure do
    { message: { content } } <- choices !! 0
    content >>= readJSON_
  if not success then do
    log result
    when revised do
      log $ "You can add context to the query by creating a file with free-form text at " <> contextPath <> " and run the migration again."
    pure $ Done $ Left unit
  else do
    log $ "The query " <> queryPath <> " was " <> if not revised then "not in need of revision." else "revised."
    pure $ Loop $ { todo: Array.drop 1 todo, done: done <> [ queryPath /\ result ] }

migrate :: Migrate -> Aff Unit
migrate info = do
  console <- liftEffect $ createConsoleInterface noCompletion
  paths <- readdir info.migrations
  let migrations = filterMap readJSON_ paths
  let migrationsAreSane = migrationsStartAt0AndIncreaseBy1 migrations
  when (not migrationsAreSane) do
    throwError $ error "Migrations must start at 0 and increase by 1."
  let meta = Path.concat [ info.migrations, "__meta" ]
  metaExists <- liftEffect $ exists meta
  when (not metaExists) do
    void $ mkdir meta
  let raw = Path.concat [ info.migrations, "__raw" ]
  rawExists <- liftEffect $ exists raw
  when (not rawExists) do
    void $ mkdir raw
  rawPaths <- readdir raw
  let rawMigrations = filterMap readJSON_ rawPaths
  when (rawMigrations /= Array.take (Array.length rawMigrations) migrations) do
    throwError $ error ("Meta files must match migrations.\nMeta: " <> show rawMigrations <> "\nMigrations: " <> show migrations)
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
    let migrationPath = Path.concat [ raw, show migrationIx ]
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
    go :: Array Int -> Aff (Step (Array Int) Unit)
    go migrationArr = do
      schema <- getSchema
      case Array.head migrationArr of
        Nothing -> pure $ Done unit
        Just migrationIx -> do
          let migrationPath = Path.concat [ info.migrations, show migrationIx ]
          migrationText <- readTextFile Encoding.UTF8 migrationPath
          let systemM = DoMigration.system
          let userM = DoMigration.user (DoMigration.Sql schema) (DoMigration.Ask migrationText)
          let isRaw = String.take 6 migrationText == "--raw\n"
          MigrationResult { result, success } <-
            if isRaw then pure $ MigrationResult { result: migrationText, success: true }
            else do
              ChatCompletionResponse { choices } <- createCompletions
                $ over ChatCompletionRequest
                    _
                      { messages =
                          [ message system systemM
                          , message user userM
                          ]
                      , response_format = pure $ ResponseFormat DoMigration.responseFormat
                      }
                    ccr
              maybe (throwError $ error "No migration could be generated") pure do
                { message: { content } } <- choices !! 0
                content >>= readJSON_
          if not success then do
            log result
            pure $ Done unit
          else do
            let
              qtext =
                """Please review the migration text inside the <migration> tag below.

<migration>
"""
                  <> result
                  <>
                    """
</migration>
Press n or N to reject and any other key to continue: """

            response <- if info.yes then pure "y" else question qtext console
            case response of
              x
                | x == "n" || x == "N" -> do
                    log "Oh noes! Please change your prompt and try again."
                    pure $ Done unit
                | otherwise -> do
                    log "Great! Creating migration 📄"
                    cmd <- try $ runSqlCommand client result mempty
                    case cmd of
                      Left _ -> do
                        log $ "It looks like the migration could not be run against the DB."
                        log $ "Please change your prompt and try again."
                        pure $ Done unit
                      Right _ -> do
                        let
                          cont = do
                            -- now it's safe to write the migration and the new queries
                            let newMigrationPath = Path.concat [ raw, show migrationIx ]
                            let metaPath = Path.concat [ meta, show migrationIx ]
                            writeTextFile Encoding.UTF8 newMigrationPath result
                            writeTextFile Encoding.UTF8 metaPath $ writeJSON
                              { migration_text_checksum: checksum migrationText
                              , result_checksum: checksum result
                              , meta_version: 0
                              }
                            pure $ Loop $ Array.drop 1 migrationArr
                        -- revise queries
                        queriesExists <- liftEffect $ exists info.queries
                        if (not queriesExists) then cont
                        else do
                          allQueryPrompts <- filter (eq <*> kebabCase) <$> readdir info.queries
                          allQueries <- filter (flip Array.elem allQueryPrompts) <$> readdir (Path.concat [ info.queries, "__raw" ])
                          newQueries' <- tailRecM
                            (goQ migrationIx info schema result)
                            { todo: allQueries, done: [] }
                          case newQueries' of
                            Left _ -> pure $ Done unit
                            Right newQueries -> do
                              for_ newQueries \(queryPath /\ queryText) ->
                                writeTextFile
                                  Encoding.UTF8
                                  (Path.concat [ info.queries, "__raw", queryPath ])
                                  queryText
                              cont
  tailRecM go (Array.drop (Array.length rawMigrations) migrations)
  closeClient client
  liftEffect $ close console
  -- at the end of a migration, we always write the human-readable schema
  schema
    { humanReadable: true
    , migrations: info.migrations
    , path: Path.concat [ info.schema, "schema.sql" ]
    }

foreign import checksum :: String -> String