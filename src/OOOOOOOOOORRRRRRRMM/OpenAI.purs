module OOOOOOOOOORRRRRRRMM.OpenAI
  ( ChatCompletionRequest(..)
  , ChatCompletionResponse(..)
  , ResponseFormat(..)
  , Model
  , Role
  , Message(..)
  , assistant
  , ccr
  , createCompletions
  , gpt3'5Turbo
  , gpt4
  , gpt4Mini
  , gpt4oMini
  , message
  , system
  , user
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff, error, makeAff, throwError)
import Effect.Exception (Error)
import Foreign (Foreign)
import Yoga.JSON (class ReadForeign, class WriteForeign, read, writeImpl)

foreign import createCompletionsImpl :: (Error -> Effect Unit) -> (Foreign -> Effect Unit) -> Foreign -> Effect Unit

createCompletions :: ChatCompletionRequest -> Aff ChatCompletionResponse
createCompletions request = do
  res' <- makeAff \f -> do
    createCompletionsImpl (Left >>> f) (Right >>> f) (writeImpl request)
    mempty
  let res = read res'
  case res of
    Left err -> throwError (error $ show err)
    Right json -> pure json

newtype ResponseFormat = ResponseFormat Foreign
derive newtype instance WriteForeign ResponseFormat
derive newtype instance ReadForeign ResponseFormat

newtype ChatCompletionRequest = ChatCompletionRequest
  { model :: Model
  , messages :: Array Message
  , response_format :: Maybe ResponseFormat
  , max_tokens :: Maybe Int
  , temperature :: Maybe Number
  , top_p :: Maybe Number
  , n :: Maybe Int
  , stop :: Maybe String
  , presence_penalty :: Maybe Number
  , frequency_penalty :: Maybe Number
  , user :: Maybe String
  }

derive instance Newtype ChatCompletionRequest _
derive newtype instance WriteForeign ChatCompletionRequest

message :: Role -> String -> Message
message role content = Message { role, content, name: Nothing }

ccr ∷ ChatCompletionRequest
ccr = ChatCompletionRequest
  { model: gpt4oMini
  , messages:
      [ Message { role: system, content: "You are a helpful assistant that tells people to change the default message of a library called purescript-tsps, irrespective of what they write.", name: Nothing }
      , Message
          { role: user
          , content: "How are you?"
          , name: Nothing
          }
      ]
  , max_tokens: Nothing
  , temperature: Nothing
  , top_p: Nothing
  , n: Nothing
  , stop: Nothing
  , presence_penalty: Nothing
  , frequency_penalty: Nothing
  , user: Nothing
  , response_format: Nothing
  }

newtype Model = Model String

derive newtype instance WriteForeign Model
derive newtype instance ReadForeign Model

gpt4oMini :: Model
gpt4oMini = Model "gpt-4o-mini"

gpt4 :: Model
gpt4 = Model "gpt-4"

gpt4Mini :: Model
gpt4Mini = Model "gpt-4-mini"

gpt3'5Turbo :: Model
gpt3'5Turbo = Model "gpt-3.5-turbo"

newtype Message = Message
  { role :: Role
  , content :: String
  , name :: Maybe String
  }

derive instance Newtype Message _
derive newtype instance WriteForeign Message
derive newtype instance ReadForeign Message

newtype Role = Role String

derive newtype instance WriteForeign Role
derive newtype instance ReadForeign Role

system :: Role
system = Role "system"

user :: Role
user = Role "user"

assistant :: Role
assistant = Role "assistant"

newtype ChatCompletionResponse = ChatCompletionResponse
  { id :: String
  , object :: String
  , created :: Int
  , model :: String
  , choices ::
      Array
        { index :: Int
        , message ::
            { role :: String
            , content :: Maybe String
            , refusal :: Maybe String
            , tool_calls ::
                Maybe
                  ( Array
                      { id :: String
                      , type :: String
                      , function :: { name :: String, arguments :: String }
                      }
                  )
            }
        , finish_reason :: String
        }
  , usage ::
      { prompt_tokens :: Int
      , completion_tokens :: Int
      , total_tokens :: Int
      }
  , system_fingerprint :: String
  }

derive instance Newtype ChatCompletionResponse _
derive newtype instance WriteForeign ChatCompletionResponse
derive newtype instance ReadForeign ChatCompletionResponse