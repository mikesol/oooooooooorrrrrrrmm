module OOOOOOOOOORRRRRRRMM.TypeScript where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (compact, filter, filterMap)
import Data.Foldable (for_, intercalate)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (over, unwrap)
import Data.String as String
import Data.String.Extra (kebabCase, pascalCase)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, error, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Foreign.Object as Object
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
import OOOOOOOOOORRRRRRRMM.Completions (ChatCompletionRequest(..), ChatCompletionResponse(..), ccr, createCompletions, message, system, user)
import OOOOOOOOOORRRRRRRMM.ConvertToResult (convertToResult)
import OOOOOOOOOORRRRRRRMM.PGInfo (InputParameter(..), OutputColumn(..), PostgresQuerySchema(..), PostgresType(..))
import OOOOOOOOOORRRRRRRMM.Pg (Database(..), Host(..), Port(..), User(..), closeClient, newClient, parsePostgresUrl, runSqlCommand)
import OOOOOOOOOORRRRRRRMM.Prompts.DoBinding as DoBinding
import OOOOOOOOOORRRRRRRMM.QueriesToRun (generateQueriesToRun)
import OOOOOOOOOORRRRRRRMM.Query.Metadata (Metadata(..))
import Yoga.JSON (class ReadForeign, readJSON, readJSON_, writeJSON)

toNonValidatedType :: PostgresType -> String
toNonValidatedType = case _ of
  PGBoolean -> "boolean"
  PGInteger -> "number"
  PGTimestamp -> "Date"
  PGTimestampz -> "Date"
  PGJson -> "Json"
  PGText -> "string"
  PGVarchar -> "string"
  PGNumeric -> "number"
  PGUuid -> "string"
  PGBytea -> "string"
  PGDate -> "Date"
  PGTime -> "Date"
  PGTimetz -> "Date"
  PGInterval -> "string"
  PGRecord -> "any"
  PGEnum -> "string"
  PGTsvector -> "string"
  PGTsquery -> "string"

toIoTsType :: PostgresType -> String
toIoTsType = case _ of
  PGBoolean -> "t.boolean"
  PGInteger -> "t.number"
  PGTimestamp -> "date"
  PGTimestampz -> "date"
  PGJson -> "json"
  PGText -> "t.string"
  PGVarchar -> "t.string"
  PGNumeric -> "t.number"
  PGUuid -> "t.string"
  PGBytea -> "t.string"
  PGDate -> "date"
  PGTime -> "date"
  PGTimetz -> "date"
  PGInterval -> "t.string"
  PGRecord -> "t.any"
  PGEnum -> "t.string"
  PGTsvector -> "t.string"
  PGTsquery -> "t.string"

toZodType :: PostgresType -> String
toZodType = case _ of
  PGBoolean -> "z.boolean()"
  PGInteger -> "z.number()"
  PGTimestamp -> "z.string()"
  PGTimestampz -> "z.date()"
  PGJson -> "json"
  PGText -> "z.string()"
  PGVarchar -> "z.string()"
  PGNumeric -> "z.number()"
  PGUuid -> "z.string()"
  PGBytea -> "z.string()"
  PGDate -> "z.date()"
  PGTime -> "z.date()"
  PGTimetz -> "z.date()"
  PGInterval -> "z.string()"
  PGRecord -> "z.any()"
  PGEnum -> "z.string()"
  PGTsvector -> "z.string()"
  PGTsquery -> "z.string()"

codegenNoValidator :: String -> PostgresQuerySchema -> String
codegenNoValidator query (PostgresQuerySchema { input: input, output: output }) = intercalate "\n"
  [ if hasJson then
      """
type JsonPrimative = string | number | boolean | null;
type JsonArray = Json[];
type JsonObject = { [key: string]: Json };
type JsonComposite = JsonArray | JsonObject;
type Json = JsonPrimative | JsonComposite;
"""
    else ""
  , iPart
  , qPart
  , oPart
  ]

  where
  hasJson = isJust $ Array.find (eq PGJson) (map (unwrap >>> _.type) (Object.values input) <> map (unwrap >>> _.type) (Object.values output))

  iPart =
    if Object.isEmpty input then ""
    else "export type I = { "
      <> String.joinWith "\n"
        ( map
            ( \(k /\ (InputParameter v)) ->
                "  " <> k <> (if v.is_nullable then "?" else "") <> ": " <> toNonValidatedType v.type <> (if v.is_array then "[]" else "") <> ";"
            )
            (Object.toUnfoldable input)
        )
      <> "\n };\n"
  qPart = "export type Q = `\n" <> query <> "\n`;\n"
  oPart =
    if Object.isEmpty output then ""
    else "export type O = { "
      <> String.joinWith "\n"
        ( map
            ( \(k /\ (OutputColumn v)) ->
                "  " <> k <> (if v.is_nullable then "?" else "") <> ": " <> toNonValidatedType v.type <> (if v.is_array then "[]" else "") <> ";"
            )
            (Object.toUnfoldable output)
        )
      <> "\n };\n"

codegenZod :: String -> PostgresQuerySchema -> String
codegenZod query (PostgresQuerySchema { input: input, output: output }) = intercalate "\n"
  [ firstPart
  , if hasJson then
      """
const literalSchema = z.union([z.string(), z.number(), z.boolean(), z.null()]);
type Literal = z.infer<typeof literalSchema>;
type Json = Literal | { [key: string]: Json } | Json[];
const json: z.ZodType<Json> = z.lazy(() =>
  z.union([literalSchema, z.array(json), z.record(json)])
);
"""
    else ""
  , iPart
  , qPart
  , oPart
  , secondPart
  ]

  where
  iIsEmpty = Object.isEmpty input
  hasJson = isJust $ Array.find (eq PGJson) (map (unwrap >>> _.type) (Object.values input) <> map (unwrap >>> _.type) (Object.values output))
  columnNamesInOrder = map fst (Array.sortWith (snd >>> unwrap >>> _.position_starting_from_1) (Object.toUnfoldable input))
  myITerm
    | Array.length columnNamesInOrder == 0 = ""
    | otherwise = String.joinWith ", "
        $ map (\i -> "$i." <> i) columnNamesInOrder
  oIsEmpty = Object.isEmpty output

  firstPart = "import { z } from 'zod';\n"
  iPart =
    if Object.isEmpty input then ""
    else "export const i = z.object({ "
      <> String.joinWith ",\n"
        ( map
            ( \(k /\ (InputParameter v)) ->
                "  " <> k <> " : " <> (if v.is_array then "z.array(" else "") <> toZodType v.type <> (if v.is_nullable then ".nullable()" else "") <> (if v.is_array then ")" else "")
            )
            (Object.toUnfoldable input)
        )
      <> "\n });\n"
  qPart = "export const q = z.literal(`\n" <> query <> "\n`);\n"
  oPart =
    if Object.isEmpty output then ""
    else "export const o = z.array(z.object({ "
      <> String.joinWith ",\n"
        ( map
            ( \(k /\ (OutputColumn v)) ->
                "  " <> k <> " : " <> (if v.is_array then "z.array(" else "") <> toZodType v.type <> (if v.is_nullable then ".nullable()" else "") <> (if v.is_array then ")" else "")
            )
            (Object.toUnfoldable output)
        )
      <> "\n }));\n"
  myIDef = if iIsEmpty then "" else "$i: z.infer<typeof i>"

  secondPart
    | oIsEmpty =
        """
export const run = (f: (s: string, v: any) => Promise<any>) => (""" <> myIDef <> """) => f(q._def.value, [""" <> myITerm <>
          """]);
"""
    | otherwise =
        """
export const run = (f: (s: string, v: any) => Promise<any>) => (""" <> myIDef <> """) => f(q._def.value, [""" <> myITerm <>
          """]).then(x => o.parse(x));
"""

codegenIoTs :: String -> PostgresQuerySchema -> String
codegenIoTs query (PostgresQuerySchema { input: input, output: output }) = intercalate "\n"
  [ firstPart
  , if hasDate then
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
  , if hasJson then
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
  , iPart
  , qPart
  , oPart
  , secondPart
  ]

  where
  iIsEmpty = Object.isEmpty input
  hasJson = isJust $ Array.find (eq PGJson) (map (unwrap >>> _.type) (Object.values input) <> map (unwrap >>> _.type) (Object.values output))
  hasDate = isJust $ Array.find (eq PGDate || eq PGTimestamp || eq PGTime || eq PGTimestampz || eq PGTimetz) (map (unwrap >>> _.type) (Object.values input) <> map (unwrap >>> _.type) (Object.values output))
  columnNamesInOrder = map fst (Array.sortWith (snd >>> unwrap >>> _.position_starting_from_1) (Object.toUnfoldable input))
  myITerm
    | Array.length columnNamesInOrder == 0 = ""
    | otherwise = String.joinWith ", "
        $ map (\i -> "$i." <> i) columnNamesInOrder
  oIsEmpty = Object.isEmpty output

  firstPart = "import * as t from 'io-ts';\n"
  iPart =
    if Object.isEmpty input then ""
    else "export const i = t.type({ "
      <> String.joinWith ",\n"
        ( map
            ( \(k /\ (InputParameter v)) ->
                "  " <> k <> " : " <> (if v.is_array then "t.array(" else "") <> ((if v.is_nullable then (\st -> "t.union([t.null," <> st <> "])") else identity) $ toIoTsType v.type) <> (if v.is_array then ")" else "")
            )
            (Object.toUnfoldable input)
        )
      <> "\n });\n"
  qPart = "export const q = t.literal(`\n" <> query <> "\n`);\n"
  oPart =
    if Object.isEmpty output then ""
    else "export const o = t.array(t.type({ "
      <> String.joinWith ",\n"
        ( map
            ( \(k /\ (OutputColumn v)) ->
                "  " <> k <> " : " <> (if v.is_array then "t.array(" else "") <> ((if v.is_nullable then (\st -> "t.union([t.null," <> st <> "])") else identity) $ toIoTsType v.type) <> (if v.is_array then ")" else "")
            )
            (Object.toUnfoldable output)
        )
      <> "\n }));\n"
  myIDef = if iIsEmpty then "" else "$i: t.TypeOf<typeof i>"

  secondPart
    | oIsEmpty =
        """
export const run = (f: (s: string, v: any) => Promise<any>) => (""" <> myIDef <> """) => f(q.value, [""" <> myITerm <>
          """]);
"""
    | otherwise =
        """
export const run = (f: (s: string, v: any) => Promise<any>) => (""" <> myIDef <> """) => f(q.value, [""" <> myITerm <>
          """]).then(x => o.decode(x));
"""

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

typescript :: Typescript -> Aff Unit
typescript info = do
  pthExists <- liftEffect $ exists info.ts
  when (not pthExists) do
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
          let systemM = DoBinding.system (DoBinding.Schema schema)
          let moduleName = pascalCase q
          let userM = DoBinding.user (DoBinding.Query rawQueryText)
          let
            getResult = do
              ChatCompletionResponse { choices } <- createCompletions info.url info.token info.additionalHeaders
                $ over ChatCompletionRequest
                    _
                      { model = info.model
                      , messages =
                          [ message system systemM
                          , message user userM
                          ]
                      }
                    ccr
              { result, success } <- maybe (throwError $ error "No PureScript could be generated") pure do
                { message: { content } } <- choices !! 0
                pure $ convertToResult content
              if not success then pure $ CodegenResult { result: (PostgresQuerySchema { input: Object.empty, output: Object.empty }), success: false, orig: result }
              else case readJSON result of
                Right (PostgresQuerySchema json) -> pure $ CodegenResult { result: PostgresQuerySchema json, success: true, orig: result }
                Left err -> do
                  log $ "Could not parse result " <> result
                  log $ "Error: " <> show err
                  log "Attempting again"
                  getResult
          CodegenResult { result, success, orig } <- getResult
          if not success then do
            log orig
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
            writeTextFile Encoding.UTF8 newModulePath $ case info.validator of
              Just Zod -> codegenZod rawQueryText result
              Just IoTs -> codegenIoTs rawQueryText result
              Nothing -> codegenNoValidator rawQueryText result
            pure $ Loop $ Array.drop 1 queryArr

  queriesToRun <- generateQueriesToRun (Path.concat <<< ([ info.ts ] <> _) <<< pure <<< (_ <> ".ts") <<< pascalCase) _.typescript_binding info meta rawQ migrations queryPaths rawQPaths
  tailRecM go $ compact queriesToRun
  closeClient client
  liftEffect $ close console

newtype CodegenResult = CodegenResult { result :: PostgresQuerySchema, success :: Boolean, orig :: String }

derive newtype instance ReadForeign CodegenResult
