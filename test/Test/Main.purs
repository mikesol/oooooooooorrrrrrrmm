module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), bracket, error, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Node.ChildProcess (SpawnOptions, closeH, errorH, spawn', stderr, stdout)
import Node.ChildProcess.Types (Exit(..))
import Node.Encoding as Encoding
import Node.Errors.SystemError (toError)
import Node.EventEmitter (once_)
import Node.FS.Aff (mkdir, readdir, rmdir, stat, unlink, writeTextFile)
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (exists)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process (cwd)
import Node.Process as Process
import Node.Stream (pipe)
import Test.Spec (around, describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import Yoga.JSON (writeJSON)

removeDirectory :: String -> Aff Unit
removeDirectory directoryPath = do
  dirExists <- liftEffect $ exists directoryPath
  when dirExists do
    files <- readdir directoryPath
    for_ files \file -> do
      let filePath = Path.concat [ directoryPath, file ]
      fstats <- stat filePath
      if isDirectory fstats then removeDirectory filePath
      else unlink filePath
    rmdir directoryPath

createTmpFolder :: Aff String
createTmpFolder = do
  rn <- liftEffect $ randomInt 0 42424242
  let fn = "/tmp/" <> writeJSON rn
  mkdir fn
  pure fn

-- for now don't remove anything
withTmpFolder :: (FilePath -> Aff Unit) -> Aff Unit
withTmpFolder = bracket createTmpFolder mempty -- removeDirectory

simpleSpawn :: String -> Array String -> (SpawnOptions -> SpawnOptions) -> Aff Unit
simpleSpawn cmd args opts = makeAff \f -> do
  cp <- spawn' cmd args opts
  pipe (stderr cp) Process.stderr
  pipe (stdout cp) Process.stdout
  cp # once_ errorH \err -> do
    f (Left $ toError err)
  cp # once_ closeH \exit -> case exit of
    Normally 0 -> f $ Right unit
    _ -> f $ Left $ error ("Process terminated with error code " <> show exit)
  mempty

main :: Effect Unit
main = launchAff_ $ runSpec' (defaultConfig { timeout = Just $ Milliseconds (60_000.0 * 5.0) }) [ consoleReporter ] do
  around withTmpFolder do
    describe "End-to-end tests" do
      it "An end-to-end test with typescript zod, typescript io-ts and purescript" \tmp -> do
        log $ "Running test in " <> tmp
        let spawnInTmp cmd args = simpleSpawn cmd args _ { cwd = Just tmp }
        spawnInTmp "pnpm" [ "init" ]
        spawnInTmp "pnpm" [ "i", "zod", "io-ts" ]
        spawnInTmp "pnpm" [ "i", "-D", "typescript", "purescript", "spago@next" ]
        spawnInTmp "pnpm" [ "tsc", "--init" ]
        spawnInTmp "pnpm" [ "spago", "init" ]
        spawnInTmp "pnpm" [ "spago", "install", "yoga-json", "foreign", "aff", "maybe" ]
        liftEffect cwd >>= \c -> spawnInTmp "pnpm" [ "i", "-D", c ]
        let migrationDir = Path.concat [tmp, "migrations"]
        let queryDir = Path.concat [tmp, "queries"]
        mkdir $ migrationDir
        mkdir $ queryDir
        -- write first migrations
        writeTextFile Encoding.UTF8 (Path.concat [migrationDir, "0"]) """
Create a user table containing a primary id,
an optional email field, a required first name field, and a
required last name field. Also create username and please make this unique.
"""
        writeTextFile Encoding.UTF8 (Path.concat [migrationDir, "1"]) """
Modify the user table to contain a verified boolean,
and set everyone so far to false. The default should also be false.

Also create a friends table that has an autoincrementing id and two columns, asker and receiver. Both should be non null with a foreign key
constraint on the user id.
"""
        writeTextFile Encoding.UTF8 (Path.concat [migrationDir, "2"]) """
Create an optional phone_number field in the user table.
"""
        -- do first migrations
        spawnInTmp "pnpm" [ "oooooooooorrrrrrrmm", "m", "-y" ]
        -- do a first query
        writeTextFile Encoding.UTF8 (Path.concat [queryDir, "insert-user"]) """
Insert a user with all of their records and using no defaults. No need to return the user.
"""
        spawnInTmp "pnpm" [ "oooooooooorrrrrrrmm", "q", "-y" ]
        writeTextFile Encoding.UTF8 (Path.concat [migrationDir, "3"]) """
Create a trigger that deletes all friend entries for a deleted user.
"""
        writeTextFile Encoding.UTF8 (Path.concat [migrationDir, "4"]) """
Add an optional image url for each user.
"""
        writeTextFile Encoding.UTF8 (Path.concat [migrationDir, "5"]) """
Add a date_joined column to users, setting the default to now if not provided. It can't be null.

Also, add a settings column to users. This column can be a json type. It can be null.
"""
        -- do follow-up migrations
        spawnInTmp "pnpm" [ "oooooooooorrrrrrrmm", "m", "-y" ]
        -- make some more queries
        writeTextFile Encoding.UTF8 (Path.concat [queryDir, "insert-user"]) """
Insert a user with all of their records and using no defaults. No need to return the user.
"""
        writeTextFile Encoding.UTF8 (Path.concat [queryDir, "users-with-one-verified-friend"]) """
Select all users with at least one friend that is verified.
"""
        writeTextFile Encoding.UTF8 (Path.concat [queryDir, "users-with-email-address"]) """
Select all users with a given email address.
"""
        -- now execute the queries
        spawnInTmp "pnpm" [ "oooooooooorrrrrrrmm", "q", "-y" ]
        -- create typescript bindings
        spawnInTmp "pnpm" [ "oooooooooorrrrrrrmm", "ts", "-p", "src/ts-raw" ]
        spawnInTmp "pnpm" [ "oooooooooorrrrrrrmm", "ts", "-p", "src/zod", "-v", "zod" ]
        spawnInTmp "pnpm" [ "oooooooooorrrrrrrmm", "ts", "-p", "src/io-ts", "-v", "io-ts" ]
        spawnInTmp "pnpm" [ "tsc", "--noEmit" ]
        -- create purescript bindings
        spawnInTmp "pnpm" [ "oooooooooorrrrrrrmm", "ps" ]
        spawnInTmp "pnpm" [ "spago", "build" ]
        -- run pre-commit hook
        spawnInTmp "pnpm" [ "oooooooooorrrrrrrmm", "pc" ]
