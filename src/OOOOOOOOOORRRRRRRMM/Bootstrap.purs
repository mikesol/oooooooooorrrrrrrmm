module OOOOOOOOOORRRRRRRMM.Bootstrap where

import Prelude

import Data.Array as Array
import Data.Filterable (compact, filterMap)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Encoding as Encoding
import Node.FS.Aff (readTextFile, readdir)
import Node.FS.Sync (exists)
import Node.Path (FilePath)
import Node.Path as Path
import Node.ReadLine (close, createConsoleInterface, noCompletion)
import OOOOOOOOOORRRRRRRMM.Arrrrrgs (Bootstrap)
import OOOOOOOOOORRRRRRRMM.Pg (ConnectionString(..), Database(..), Host(..), Port(..), User(..), closeClient, newClientCS, runSqlCommand)
import Yoga.JSON (class ReadForeign, readJSON_, writeJSON)

newtype SchemaResult = SchemaResult { result :: String, success :: Boolean }

derive newtype instance ReadForeign SchemaResult

startInstanceCmd :: String -> String
startInstanceCmd = append "pg_tmp "

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

bootstrap :: Bootstrap -> Aff Unit
bootstrap info = do
  console <- liftEffect $ createConsoleInterface noCompletion
  migrationPaths <- readdir info.migrations
  let migrations = filterMap readJSON_ migrationPaths
  let migrationsAreSane = migrationsStartAt0AndIncreaseBy1 migrations
  when (not migrationsAreSane) do
    throwError $ error "Migrations must start at 0 and increase by 1."
  let rawM = Path.concat [ info.migrations, "__raw" ]
  rawMExists <- liftEffect $ exists rawM
  rawMigrations' <- if not rawMExists then pure [] else readdir rawM
  let (rawMigrations :: Array Int) = Array.sort $ compact $ map readJSON_ rawMigrations'
  log "Starting postgres 🤓"
  client <- newClientCS $ ConnectionString info.connectionString
  for_ rawMigrations \migrationIx -> do
    let migrationPath = Path.concat [ rawM, writeJSON migrationIx ]
    log $ "Infusing db with migration in " <> migrationPath
    sql <- readTextFile Encoding.UTF8 migrationPath
    void $ runSqlCommand client sql mempty
  closeClient client
  liftEffect $ close console
