module OOOOOOOOOORRRRRRRMM.Pg where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, makeAff)
import Foreign (Foreign)

data Client

data PGVars

foreign import pgVarsAppend :: PGVars -> PGVars -> PGVars

instance Semigroup PGVars where
  append = pgVarsAppend

foreign import pgVarsMempty :: PGVars

instance Monoid PGVars where
  mempty = pgVarsMempty

foreign import purePgVars :: forall a. a -> PGVars

foreign import newClientImpl
  :: (Error -> Effect Unit) -> (Client -> Effect Unit) -> Host -> Port -> User -> Database -> Effect Unit

foreign import newClientConnectionStringImpl
  :: (Error -> Effect Unit) -> (Client -> Effect Unit) -> ConnectionString -> Effect Unit

newtype Host = Host String
newtype Port = Port Int
newtype User = User String
newtype Database = Database String
newtype ConnectionString = ConnectionString String

newClientCS :: ConnectionString -> Aff Client
newClientCS cs = makeAff \f -> do
  newClientConnectionStringImpl (f <<< Left) (f <<< Right) cs
  mempty

newClient :: Host -> Port -> User -> Database -> Aff Client
newClient host port string database = makeAff \f -> do
  newClientImpl (f <<< Left) (f <<< Right) host port string database
  mempty

foreign import runSqlCommandImpl
  :: (Error -> Effect Unit) -> (Foreign -> Effect Unit) -> Client -> String -> PGVars -> Effect Unit

runSqlCommand :: Client -> String -> PGVars -> Aff Foreign
runSqlCommand client sql values = makeAff \f -> do
  runSqlCommandImpl (f <<< Left) (f <<< Right) client sql values
  mempty

foreign import closeClientImpl
  :: (Error -> Effect Unit) -> Effect Unit -> Client -> Effect Unit

closeClient :: Client -> Aff Unit
closeClient client = makeAff \f -> do
  closeClientImpl (f <<< Left) (f $ Right unit) client
  mempty

type PGInfo =
  { host :: Host
  , port :: Port
  , database :: Database
  , user :: User
  }

foreign import parsePostgresUrlImpl
  :: Maybe PGInfo
  -> (PGInfo -> Maybe PGInfo)
  -> String
  -> Maybe PGInfo

parsePostgresUrl :: String -> Maybe PGInfo
parsePostgresUrl = parsePostgresUrlImpl Nothing Just