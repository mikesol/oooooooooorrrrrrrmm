module OOOOOOOOOORRRRRRRMM.QueriesToRun where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Node.Encoding as Encoding
import Node.FS.Aff (readTextFile)
import Node.FS.Sync (exists)
import Node.Path as Path
import OOOOOOOOOORRRRRRRMM.Checksum (checksum)
import OOOOOOOOOORRRRRRRMM.Query.Metadata (Metadata(..))
import Yoga.JSON (readJSON_, writeJSON)

generateQueriesToRun :: _ -> _ -> String -> String -> Array String -> Array String -> Array String -> Aff (Array (Maybe String))
generateQueriesToRun fff info meta rawQ migrationPaths queryPaths rawQPaths = do
  mostRecentMigration <- readTextFile Encoding.UTF8 $ Path.concat [ info.migrations, writeJSON (Array.length migrationPaths - 1) ]
  let mostRecentMigrationChecksum = checksum mostRecentMigration
  let failureCase = throwError $ error "Please update your queries using `oooooooooorrrrrrrmm q` before attempting to create bindings."
  for queryPaths \qp -> do
    let runMe = pure $ Just qp
    if not (Array.elem qp rawQPaths) then failureCase
    else do
      qt <- readTextFile Encoding.UTF8 $ Path.concat [ info.queries, qp ]
      let metaPath = Path.concat [ meta, qp ]
      let rawPath = Path.concat [ rawQ, qp ]
      pe <- liftEffect $ exists metaPath
      if not pe then failureCase
      else do
        metadata <- readTextFile Encoding.UTF8 metaPath
        case readJSON_ metadata of
          Nothing -> failureCase
          Just (Metadata md@{ query_text_checksum, result_checksum, most_recent_migration_checksum }) -> do
            if mostRecentMigrationChecksum /= most_recent_migration_checksum then failureCase
            else do
              rw <- liftEffect $ exists rawPath
              if not rw then failureCase
              else if query_text_checksum /= checksum qt then failureCase
              else do
                rt <- readTextFile Encoding.UTF8 rawPath
                if result_checksum /= checksum rt then failureCase
                else case fff md of
                  Nothing -> runMe
                  Just pb -> if pb.result_checksum == result_checksum && pb.query_text_checksum == query_text_checksum then pure Nothing else runMe
