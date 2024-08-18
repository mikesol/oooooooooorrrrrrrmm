module OOOOOOOOOORRRRRRRMM.PureScript where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (fold, (!!), (..))
import Data.Array as Array
import Data.CodePoint.Unicode (isAlphaNum, isUpper)
import Data.Either (Either(..))
import Data.Filterable (compact, filter, filterMap)
import Data.Foldable (for_, intercalate)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (over, unwrap)
import Data.String (codePointFromChar)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
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
import Node.FS.Aff (mkdir, mkdir', readTextFile, readdir, writeTextFile)
import Node.FS.Perms (permsAll)
import Node.FS.Sync (exists)
import Node.Path (FilePath)
import Node.Path as Path
import Node.ReadLine (close, createConsoleInterface, noCompletion)
import OOOOOOOOOORRRRRRRMM.Arrrrrgs (PureScript)
import OOOOOOOOOORRRRRRRMM.Checksum (checksum)
import OOOOOOOOOORRRRRRRMM.Completions (ChatCompletionRequest(..), ChatCompletionResponse(..), ccr, createCompletions, message, system, user)
import OOOOOOOOOORRRRRRRMM.ConvertToResult (convertToResult)
import OOOOOOOOOORRRRRRRMM.PGInfo (InputParameter(..), OutputColumn(..), PostgresQuerySchema(..), PostgresType(..))
import OOOOOOOOOORRRRRRRMM.Pg (Database(..), Host(..), Port(..), User(..), closeClient, newClient, parsePostgresUrl, runSqlCommand)
import OOOOOOOOOORRRRRRRMM.Prompts.DoBinding as DoBinding
import OOOOOOOOOORRRRRRRMM.QueriesToRun (generateQueriesToRun)
import OOOOOOOOOORRRRRRRMM.Query.Metadata (Metadata(..))
import Safe.Coerce (coerce)
import Yoga.JSON (class ReadForeign, readJSON, readJSON_, writeJSON)

removeLLMGeneratedImports :: String -> String
removeLLMGeneratedImports s = String.joinWith "\n" withoutImports
  where
  split = String.split (String.Pattern "\n") s
  withoutImports = Array.filter (notEq "import " <<< String.take 7) split

toPsType :: PostgresType -> String
toPsType = case _ of
  PGInteger -> "Int"
  PGBoolean -> "Boolean"
  PGTimestamp -> "JSDate"
  PGTimestampz -> "JSDate"
  PGJson -> "Foreign"
  PGText -> "String"
  PGVarchar -> "String"
  PGNumeric -> "Number"
  PGUuid -> "String"
  PGBytea -> "String"
  PGDate -> "JSDate"
  PGTime -> "JSDate"
  PGTimetz -> "JSDate"
  PGInterval -> "String"
  PGRecord -> "Foreign"
  PGEnum -> "String"
  PGTsvector -> "String"
  PGTsquery -> "String"

codegen :: String -> String -> PostgresQuerySchema -> String
codegen moduleName query (PostgresQuerySchema { input: input, output: output }) = intercalate "\n" [ firstPart, iPart, qPart, oPart, secondPart ]

  where
  iIsEmpty = Object.isEmpty input
  inputType = if iIsEmpty then "" else " I ->"
  inputVar = if iIsEmpty then "" else " i "
  columnNamesInOrder = map fst (Array.sortWith (snd >>> unwrap >>> _.position_starting_from_1) (Object.toUnfoldable input))
  inputImpl
    | Array.length columnNamesInOrder == 0 = ""
    | otherwise = String.joinWith ", "
        $ map (\i -> "writeImpl i." <> i) columnNamesInOrder
  oIsEmpty = Object.isEmpty output
  hasDate = isJust $ Array.find (eq PGDate || eq PGTimestamp || eq PGTime || eq PGTimestampz || eq PGTimetz) (map (unwrap >>> _.type) (Object.values input) <> map (unwrap >>> _.type) (Object.values output))
  hasMaybe = Array.elem true (map (unwrap >>> _.is_nullable) (Object.values input) <> map (unwrap >>> _.is_nullable) (Object.values output))
  jsonImport = if oIsEmpty then "import Yoga.JSON (writeImpl)\n" else "import Yoga.JSON (writeImpl, E, read)\n"
  dateImport = if hasDate then "import Data.JSDate (JSDate)\n" else ""
  maybeImport = if hasMaybe then "import Data.Maybe (Maybe)\n" else ""

  firstPart = "module " <> moduleName <> " where\n\nimport Prelude\nimport Effect.Aff (Aff)\nimport Data.Symbol (reflectSymbol)\nimport Type.Proxy (Proxy(..))\nimport Foreign (Foreign)\n"
    <> jsonImport
    <> dateImport
    <> maybeImport
  iPart =
    if Object.isEmpty input then ""
    else "type I = { "
      <> String.joinWith ",\n"
        ( map
            ( \(k /\ (InputParameter v)) ->
                let
                  needsParens = v.is_nullable && v.is_array
                in
                  "  " <> k <> " :: " <> (if v.is_array then "Array " else "") <> (if needsParens then "(" else "") <> (if v.is_nullable then "Maybe " else "") <> toPsType v.type <> (if needsParens then ")" else "")
            )
            (Object.toUnfoldable input)
        )
      <> "\n }\n"
  qPart = "type Q = \"\"\"\n" <> query <> "\n\"\"\"\n"
  oPart =
    if Object.isEmpty output then ""
    else "type O = Array { "
      <> String.joinWith ",\n"
        ( map
            ( \(k /\ (OutputColumn v)) ->
                let
                  needsParens = v.is_nullable && v.is_array
                in
                  "  " <> k <> " :: " <> (if v.is_array then "Array " else "") <> (if needsParens then "(" else "") <> (if v.is_nullable then "Maybe " else "") <> toPsType v.type <> (if needsParens then ")" else "")
            )
            (Object.toUnfoldable output)
        )
      <> "\n }\n"
  secondPart
    | oIsEmpty =
        """
run :: (String -> Foreign -> Aff Foreign) ->""" <> inputType
          <>
            """ Aff Unit
run go """
          <> inputVar
          <>
            """= do
  void $ go (reflectSymbol (Proxy :: _ Q)) $ writeImpl ([ """
          <> inputImpl
          <>
            """ ] :: Array Foreign)
"""
    | otherwise =
        """
run :: (String -> Foreign -> Aff Foreign) ->""" <> inputType
          <>
            """ Aff (E O)
run go """
          <> inputVar
          <>
            """= do
  o <- go (reflectSymbol (Proxy :: _ Q)) $ writeImpl ([ """
          <> inputImpl
          <>
            """] :: Array Foreign)
  pure $ read o
"""

newtype CodegenResult = CodegenResult { result :: PostgresQuerySchema, success :: Boolean, orig :: String }

derive newtype instance ReadForeign CodegenResult

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

pureScript :: PureScript -> Aff Unit
pureScript info = do
  let splitPrefix = String.split (String.Pattern ".") info.prefix
  let
    prefixIsValid = append (Conj (Array.length splitPrefix > 0))
      $ fold
      $ map
          ( toCharArray
              >>> map codePointFromChar
              >>> \a -> Conj (Array.length a > 0) <> Conj ((isUpper <$> (a !! 0)) == Just true)
                <> fold (map (isAlphaNum >>> Conj) $ Array.drop 1 a)
          )
          splitPrefix
  when (not $ coerce prefixIsValid) do
    throwError $ error "The prefix must be a valid purescript module name."
  let longPath = Path.concat $ [ info.ps ] <> splitPrefix
  pthExists <- liftEffect $ exists longPath
  when (not pthExists) do
    void $ mkdir' longPath { mode: permsAll, recursive: true }
  for_ (1 .. Array.length splitPrefix) \ix -> do
    let xpth = Path.concat $ [ info.ps ] <> Array.take ix splitPrefix
    xpthExists <- liftEffect $ exists xpth
    when (not xpthExists) do
      void $ mkdir xpth
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
          let moduleFileName = pascalCase q
          let moduleName = info.prefix <> "." <> pascalCase q
          let userM = DoBinding.user (DoBinding.Query rawQueryText)
          let
            getResult = do
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
              { result, success } <- maybe (throwError $ error "No PureScript could be generated") pure do
                { message: { content } } <- choices !! 0
                pure $ convertToResult content
              if not success then pure $ CodegenResult { result: (PostgresQuerySchema { input: Object.empty, output: Object.empty }), success: false, orig: result }
              else case readJSON result of
                Right (PostgresQuerySchema json) -> pure $ CodegenResult { result: PostgresQuerySchema json, success: true, orig: result }
                Left err -> do
                  log $ "Could not parse query, attempting again" <> show err
                  getResult
          CodegenResult { result, success, orig } <- getResult
          if not success then do
            log orig
            pure $ Done unit
          else do
            log "Creating PureScript file"
            -- now it's safe to write the query
            let newModulePath = Path.concat ([ info.ps ] <> splitPrefix <> [ moduleFileName <> ".purs" ])
            let metaPath = Path.concat [ info.queries, "__meta", q ]
            rawPreviousMeta <- readTextFile Encoding.UTF8 metaPath
            let queryPath = Path.concat [ info.queries, q ]
            intentionText <- readTextFile Encoding.UTF8 queryPath
            case readJSON_ rawPreviousMeta of
              Just (Metadata { typescript_binding, most_recent_migration_checksum, meta_version, query_text_checksum, result_checksum }) -> writeTextFile Encoding.UTF8 metaPath $ writeJSON $ Metadata
                { query_text_checksum
                , result_checksum
                , meta_version
                , purescript_binding: Just
                    { query_text_checksum: checksum intentionText
                    , result_checksum: checksum rawQueryText
                    }
                , typescript_binding
                , most_recent_migration_checksum
                }
              Nothing -> throwError $ error "Could not read meta file"
            writeTextFile Encoding.UTF8 newModulePath $ codegen moduleName rawQueryText result
            pure $ Loop $ Array.drop 1 queryArr

  queriesToRun <- generateQueriesToRun (Path.concat <<< (([ info.ps ] <> splitPrefix) <> _) <<< pure <<< (_ <> ".purs") <<< pascalCase) _.purescript_binding info meta rawQ migrations queryPaths rawQPaths
  tailRecM go $ compact queriesToRun
  closeClient client
  liftEffect $ close console
