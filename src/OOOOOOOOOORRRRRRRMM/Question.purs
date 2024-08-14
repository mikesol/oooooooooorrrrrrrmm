module OOOOOOOOOORRRRRRRMM.Question where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over)
import Effect.Aff (Aff, error, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Node.Buffer (toString)
import Node.ChildProcess (exec')
import Node.Encoding (Encoding(..))
import Node.Encoding as Encoding
import Node.FS.Aff (readTextFile, readdir)
import Node.FS.Sync (exists)
import Node.Path (FilePath)
import Node.Path as Path
import Node.ReadLine (close, createConsoleInterface, noCompletion)
import OOOOOOOOOORRRRRRRMM.Arrrrrgs (Question)
import OOOOOOOOOORRRRRRRMM.OpenAI (ChatCompletionRequest(..), ChatCompletionResponse(..), ResponseFormat(..), ccr, createCompletions, message, system, user)
import OOOOOOOOOORRRRRRRMM.Pg (Database(..), Host(..), Port(..), User(..), closeClient, newClient, parsePostgresUrl, runSqlCommand)
import OOOOOOOOOORRRRRRRMM.Prompts.DoQuestion as DoQuestion
import Yoga.JSON (class ReadForeign, readJSON_, writeJSON)

newtype QuestionResult = QuestionResult { result :: String, success :: Boolean }

derive newtype instance ReadForeign QuestionResult

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

question :: Question -> Aff Unit
question info = do
  console <- liftEffect $ createConsoleInterface noCompletion
  migrationPaths <- readdir info.migrations
  let migrations = filterMap readJSON_ migrationPaths
  let migrationsAreSane = migrationsStartAt0AndIncreaseBy1 migrations
  when (not migrationsAreSane) do
    throwError $ error "Migrations must start at 0 and increase by 1."
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
  rn <- liftEffect $ randomInt 0 42424242
  let schemaPath =  Path.concat [ "/tmp", "schema-" <> writeJSON rn <> ".sql" ] 
  makeAff \f -> do
    void $ exec' (pgDumpCmd parsed.database parsed.host parsed.port parsed.user schemaPath) identity $ \{ error: e } -> do
      case e of
        Nothing -> f $ Right unit
        Just _ -> f $ Left $ error $ "Could not dump the pg database during the migration 😔 " <> show e
    mempty
  sc <- readTextFile Encoding.UTF8 schemaPath
  let systemM = DoQuestion.system
  let userM = DoQuestion.user (DoQuestion.Schema sc) (DoQuestion.Question info.question)
  ChatCompletionResponse { choices } <- createCompletions
    $ over ChatCompletionRequest
        _
          { messages =
              [ message system systemM
              , message user userM
              ]
          , response_format = pure $ ResponseFormat DoQuestion.responseFormat
          }
        ccr
  QuestionResult { result, success } <- maybe (throwError $ error "No migration could be generated") pure do
    { message: { content } } <- choices !! 0
    content >>= readJSON_
  if not success then do
    log result
  else do
    log "#####"
    log result
  closeClient client
  liftEffect $ close console
