module OOOOOOOOOORRRRRRRMM.TypeScript where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (compact, filter, filterMap)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (over)
import Data.String as String
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
import OOOOOOOOOORRRRRRRMM.Arrrrrgs (Typescript, Validator(..))
import OOOOOOOOOORRRRRRRMM.Checksum (checksum)
import OOOOOOOOOORRRRRRRMM.OpenAI (ChatCompletionRequest(..), ChatCompletionResponse(..), ResponseFormat(..), ccr, createCompletions, message, system, user)
import OOOOOOOOOORRRRRRRMM.Pg (Database(..), Host(..), Port(..), User(..), closeClient, newClient, parsePostgresUrl, runSqlCommand)
import OOOOOOOOOORRRRRRRMM.Prompts.DoTypescript as DoTypescript
import OOOOOOOOOORRRRRRRMM.Prompts.DoTypescript.DoIoTs as DoIoTs
import OOOOOOOOOORRRRRRRMM.Prompts.DoTypescript.DoZod as DoZod
import OOOOOOOOOORRRRRRRMM.QueriesToRun (generateQueriesToRun)
import OOOOOOOOOORRRRRRRMM.Query.Metadata (Metadata(..))
import Yoga.JSON (class ReadForeign, readJSON_, writeJSON)

newtype QueryResult = QueryResult { result :: String, success :: Boolean }

derive newtype instance ReadForeign QueryResult

dehallucinate :: Maybe Validator -> String -> String
dehallucinate mv ss = go mv
  $ String.replaceAll (String.Pattern "```") (String.Replacement "")
  $ String.replaceAll (String.Pattern "<typescript>") (String.Replacement "")
  $ String.replaceAll (String.Pattern "</typescript>") (String.Replacement "")
  $ String.replaceAll (String.Pattern "```typescript") (String.Replacement "") ss
  where
  go (Just Zod) s = (if zodIsImported then "" else "import { z } from 'zod';")
    <>
      ( if usesJson then
          """
const literalSchema = z.union([z.string(), z.number(), z.boolean(), z.null()]);
type Literal = z.infer<typeof literalSchema>;
type Json = Literal | { [key: string]: Json } | Json[];
const json: z.ZodType<Json> = z.lazy(() =>
  z.union([literalSchema, z.array(json), z.record(json)])
);

"""
        else ""
      )
    <> String.replaceAll (String.Pattern "z.json()") (String.Replacement "json") s
    where
    zodIsImported = isJust (String.indexOf (String.Pattern "from 'zod'") s) || isJust (String.indexOf (String.Pattern "from \"zod\"") s)
    usesJson = isJust (String.indexOf (String.Pattern "z.json") s)
  go (Just IoTs) s = (if ioTsIsImported then "" else "import * as t from 'io-ts';")
    <>
      ( if usesDate then
          """
function isInstanceOf<T>(ctor: new (...args: any[]) => T) {
  return new t.Type<T, T, unknown>(
    'InstanceOf',
    (u: unknown): u is T => u instanceof ctor,
    (u, c) => (u instanceof ctor ? t.success(u) : t.failure(u, c)),
    t.identity
  );
}

const date = isInstanceOf(Date);

"""
        else ""
      )
    <>
      ( if usesJson then
          """
type JsonPrimative = string | number | boolean | null;
type JsonArray = Json[];
type JsonObject = { [key: string]: Json };
type JsonComposite = JsonArray | JsonObject;
type Json = JsonPrimative | JsonComposite;

const json: t.Type<Json> = t.recursion('Json', () =>
  t.union([t.null, t.boolean, t.string, t.number, t.array(json), t.record(t.string, json)])
)
"""
        else ""
      )
    <> String.replaceAll (String.Pattern "t.json") (String.Replacement "json")
      ( String.replaceAll (String.Pattern "t.date") (String.Replacement "date") s)
    where
    ioTsIsImported = isJust (String.indexOf (String.Pattern "from 'io-ts'") s) || isJust (String.indexOf (String.Pattern "from \"io-ts\"") s)
    usesJson = isJust (String.indexOf (String.Pattern "t.json") s)
    usesDate = isJust (String.indexOf (String.Pattern "t.date") s)
  go Nothing s = s

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

typescript :: Typescript -> Aff Unit
typescript info = do
  pthExists <- liftEffect $ exists info.ts
  when (not pthExists)  do
    void $ mkdir info.ts
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
          let rawQueryPath = Path.concat [ info.queries, "__raw", q ]
          log $ "Reading query from " <> rawQueryPath
          rawQueryText <- readTextFile Encoding.UTF8 rawQueryPath
          let
            systemM = case info.validator of
              Just Zod -> DoZod.system
              Just IoTs -> DoIoTs.system
              Nothing -> DoTypescript.system
          let moduleName = pascalCase q
          let
            userM = case info.validator of
              Just Zod -> DoZod.user (DoZod.Schema schema) (DoZod.Query rawQueryText)
              Just IoTs -> DoIoTs.user (DoIoTs.Schema schema) (DoIoTs.Query rawQueryText)
              Nothing -> DoTypescript.user (DoTypescript.Schema schema) (DoTypescript.Query rawQueryText)
          ChatCompletionResponse { choices } <- createCompletions
            $ over ChatCompletionRequest
                _
                  { messages =
                      [ message system systemM
                      , message user userM
                      ]
                  , response_format = pure $ ResponseFormat DoTypescript.responseFormat
                  }
                ccr
          QueryResult { result, success } <- maybe (throwError $ error "No typescript could be generated") pure do
            { message: { content } } <- choices !! 0
            content >>= readJSON_
          if not success then do
            log result
            pure $ Done unit
          else do
            log "Creating Typescript file"
            -- now it's safe to write the query
            let metaPath = Path.concat [ info.queries, "__meta", q ]
            rawPreviousMeta <- readTextFile Encoding.UTF8 metaPath
            let queryPath = Path.concat [ info.queries, q ]
            intentionText <- readTextFile Encoding.UTF8 queryPath
            case readJSON_ rawPreviousMeta of
              Just (Metadata { purescript_binding, most_recent_migration_checksum, meta_version, query_text_checksum, result_checksum }) -> writeTextFile Encoding.UTF8 metaPath $ writeJSON $ Metadata
                { query_text_checksum
                , result_checksum
                , meta_version
                , typescript_binding: Just
                    { query_text_checksum: checksum intentionText
                    , result_checksum: checksum rawQueryText
                    }
                , purescript_binding
                , most_recent_migration_checksum
                }
              Nothing -> throwError $ error "Could not read meta file"
            let newModulePath = Path.concat ([ info.ts ] <> [ moduleName <> ".ts" ])
            writeTextFile Encoding.UTF8 newModulePath $ dehallucinate info.validator result
            pure $ Loop $ Array.drop 1 queryArr

  queriesToRun <- generateQueriesToRun ( Path.concat <<< ([ info.ts ] <> _) <<< pure <<< (_ <> ".ts") <<< pascalCase) _.typescript_binding info meta rawQ migrations queryPaths rawQPaths
  tailRecM go $ compact queriesToRun
  closeClient client
  liftEffect $ close console
