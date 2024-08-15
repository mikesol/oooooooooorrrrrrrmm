module OOOOOOOOOORRRRRRRMM.PreCommit where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (for_)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Node.Encoding as Encoding
import Node.FS.Aff (readTextFile, readdir)
import Node.FS.Sync (exists)
import Node.Path as Path
import OOOOOOOOOORRRRRRRMM.Arrrrrgs (PreCommit)
import OOOOOOOOOORRRRRRRMM.Checksum (checksum)
import OOOOOOOOOORRRRRRRMM.Query.Metadata (Metadata(..))
import Yoga.JSON (readJSON, writeJSON)

noRawNoMetaAndSorted :: Array String -> Array String
noRawNoMetaAndSorted = Array.sort <<< filter (_ /= "__raw") <<< filter (_ /= "__meta")

preCommit :: PreCommit -> Aff Unit
preCommit info = do
  let
    migrationPath = info.migrations
    migrationRawPath = Path.concat [ info.migrations, "__raw" ]
    migrationMetaPath = Path.concat [ info.migrations, "__meta" ]
    queryPath = info.queries
    queryRawPath = Path.concat [ info.queries, "__raw" ]
    queryMetaPath = Path.concat [ info.queries, "__meta" ]
  queriesExist <- liftEffect $ exists info.queries
  when (not queriesExist) do
    throwError $ error "The pre-commit hook requires a queries directory to exist. Even if it is empty, please create one and check it into version control."
  migrationFiles <- noRawNoMetaAndSorted <$> readdir migrationPath
  migrationMetaFiles <- noRawNoMetaAndSorted <$> readdir migrationMetaPath
  migrationRawFiles <- noRawNoMetaAndSorted <$> readdir migrationRawPath
  queryFiles <- noRawNoMetaAndSorted <$>readdir queryPath
  queryMetaFiles <-noRawNoMetaAndSorted <$> readdir queryMetaPath
  queryRawFiles <- noRawNoMetaAndSorted <$>readdir queryRawPath
  when ((migrationFiles /= migrationMetaFiles) || (migrationFiles /= migrationRawFiles)) do
    throwError $ error $
      """Migration files are inconsistent:
User-created migration files: """ <> writeJSON migrationFiles
        <>
          """
Generated migrations: """
        <> writeJSON migrationRawFiles
        <>
          """
Metadata: """
        <> writeJSON migrationMetaFiles
        <>
          """

Please don't check this into vc. The best way to fix this is to revert whatever work you've just done.

If that's not possible or that won't fix the problem, there's a chance your DB may be out of sync with the representation here.

The best way to fix this is to run `pg_dump` on your db, delete all of the migrations as well as the __raw and __meta folder, and create a single migration with the current `pg_dump` and a single-line comment `--raw` at the top of the file.
"""
  let
    nMigrations = Array.length migrationFiles
  when ((queryFiles /= queryMetaFiles) || (queryFiles /= queryRawFiles)) do
    throwError $ error $
      """Query files are inconsistent:
User-created query files: """ <> writeJSON queryFiles
        <>
          """
Generated queries: """
        <> writeJSON queryRawFiles
        <>
          """
Metadata: """
        <> writeJSON queryMetaFiles
        <>
          """

Please don't check this into vc. The best way to fix this is to revert whatever work you've just done.
If that's not possible or that won't fix the problem, the best way to fix this is to delete the `__raw` and `__meta` folders from your query and run `oooooooooorrrrrrrmm q` to regenerate the queries. Also make sure to regenerate your language bindings if you have any.
"""
  let mrmPath = Path.concat [ info.migrations, "__raw", writeJSON (nMigrations - 1) ]
  mostRecentMigration <- readTextFile Encoding.UTF8 mrmPath
  for_ queryMetaFiles \queryMetaFile -> do
    queryMeta <- readTextFile Encoding.UTF8 $ Path.concat [ queryMetaPath, queryMetaFile ]
    case readJSON queryMeta of
      Left e -> throwError $ error ("Could not read query file " <> queryMetaFile <> " because of " <> show e)
      Right (Metadata m) -> do
        when (m.meta_version /= 0) do
          throwError $ error ("Expecting meta version of 0 but found " <> writeJSON m.meta_version <> ". Please upgrade to a newer version of oooooooooorrrrrrrmm.")
        when (m.most_recent_migration_checksum /= checksum mostRecentMigration) do
          throwError $ error
            ( """The query """ <> queryMetaFile <>
                """ was likely built against an incomplete set of migrations.
The best way to fix this is to run `oooooooooorrrrrrrmm q`, using the appropriate query directory. These will regenerate your out-of-date queries."""
            )
        queryText <- readTextFile Encoding.UTF8 $ Path.concat [ info.queries, queryMetaFile ]
        rawQueryText <- readTextFile Encoding.UTF8 $ Path.concat [ info.queries, "__raw", queryMetaFile ]
        when (m.query_text_checksum /= checksum queryText) do
          throwError $ error
            ( """The original text of """ <> queryMetaFile <>
                """ has been updated.
The best way to fix this is to run `oooooooooorrrrrrrmm q`, using the appropriate query directory. These will regenerate your out-of-date queries."""
            )
        when (m.result_checksum /= checksum rawQueryText) do
          throwError $ error
            ( """The raw query of """ <> queryMetaFile <>
                """ has been updated.
The best way to fix this is to run `oooooooooorrrrrrrmm q`, using the appropriate query directory. These will regenerate your out-of-date queries."""
            )
        for_ m.purescript_binding \p -> do
          when (p.query_text_checksum /= m.query_text_checksum || p.result_checksum /= m.result_checksum) do
            throwError $ error
              ( """The purescript binding for """ <> queryMetaFile <>
                  """ is likely out-of-date.
The best way to fix this is to run `oooooooooorrrrrrrmm ps`, using the appropriate query directory. These will regenerate your out-of-date bindings."""
              )
        for_ m.typescript_binding \t -> do
          when (t.query_text_checksum /= m.query_text_checksum || t.result_checksum /= m.result_checksum) do
            throwError $ error
              ( """The typescript binding for """ <> queryMetaFile <>
                  """ is likely out-of-date.
The best way to fix this is to run `oooooooooorrrrrrrmm ts`, using the appropriate query directory. These will regenerate your out-of-date bindings."""
              )
