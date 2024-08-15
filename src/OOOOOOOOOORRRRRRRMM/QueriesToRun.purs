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

generateQueriesToRun :: (String ->String) -> _ -> _ -> String -> String -> Array Int -> Array String -> Array String -> Aff (Array (Maybe String))
generateQueriesToRun makeFilename fff info meta rawQ migrations queryPaths rawQPaths = do
  mostRecentMigration <- readTextFile Encoding.UTF8 $ Path.concat [ info.migrations, "__raw", writeJSON (Array.length migrations - 1) ]
  let mostRecentMigrationChecksum = checksum mostRecentMigration
  let failureCase s = throwError $ error $ s <> " Please update your queries using `oooooooooorrrrrrrmm q` before attempting to create bindings."
  for queryPaths \qp -> do
    let runMe = pure $ Just qp
    if not (Array.elem qp rawQPaths) then failureCase $ "Could not find a query with the name " <> qp <> "."
    else do
      qt <- readTextFile Encoding.UTF8 $ Path.concat [ info.queries, qp ]
      let metaPath = Path.concat [ meta, qp ]
      let rawPath = Path.concat [ rawQ, qp ]
      pe <- liftEffect $ exists metaPath
      if not pe then failureCase $ "No metadata has been generated for " <> qp <> "."
      else do
        metadata <- readTextFile Encoding.UTF8 metaPath
        case readJSON_ metadata of
          Nothing -> failureCase $ "Could not parse query metadata for " <> qp <> "."
          Just (Metadata md@{ query_text_checksum, result_checksum, most_recent_migration_checksum }) -> do
            if mostRecentMigrationChecksum /= most_recent_migration_checksum then failureCase $ "The checksum for the most recent migration is incorrect."
            else do
              rw <- liftEffect $ exists rawPath
              if not rw then failureCase $ "A raw query has not been generated for " <> qp <> "."
              else if query_text_checksum /= checksum qt then failureCase $ "The checksum for the original text of query " <> qp <> " is inconsistent."
              else do
                rt <- readTextFile Encoding.UTF8 rawPath
                if result_checksum /= checksum rt then failureCase $ "The checksum for the generated query for " <> qp <> " is inconsistent."
                else case fff md of
                  Nothing -> runMe
                  Just pb -> do
                    fex <- liftEffect $ exists $ makeFilename qp
                    if not fex then runMe
                    else if pb.result_checksum == result_checksum && pb.query_text_checksum == query_text_checksum then pure Nothing
                    else runMe
