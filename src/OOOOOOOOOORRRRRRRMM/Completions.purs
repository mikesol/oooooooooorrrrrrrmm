module OOOOOOOOOORRRRRRRMM.Completions
  ( ChatCompletionRequest(..)
  , ChatCompletionResponse(..)
  , Role
  , Message(..)
  , assistant
  , ccr
  , createCompletions
  , message
  , system
  , user
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, error, throwError)
import Foreign.Object (Object, fromFoldable)
import Foreign.Object as Object
import Yoga.Fetch (URL(..), fetch, json, postMethod, statusCode)
import Yoga.Fetch.Impl.Node (nodeFetch)
import Yoga.JSON (class ReadForeign, class WriteForeign, read, writeJSON)

createCompletions :: String -> Maybe String -> Object String -> ChatCompletionRequest -> Aff ChatCompletionResponse
createCompletions url token additionalHeaders request = do
  let
    headers = Object.union additionalHeaders $ fromFoldable
      $ [ "Content-Type" /\ "application/json" ] <> maybe [] (\t -> [ "Authorization" /\ ("Bearer " <> t) ]) token
  let err = append ("Failed to parse response at url " <> url <> " with headers " <> writeJSON headers <> " :: ")
  r <- fetch
    nodeFetch
    (URL url)
    { method: postMethod
    , headers
    , body: writeJSON request
    }
  case statusCode r of
    200 -> json r >>= read >>> case _ of
      Left e -> throwError $ error $ err $ show e
      Right a -> pure a
    _ -> throwError $ error $ err $ show (statusCode r)

newtype ChatCompletionRequest = ChatCompletionRequest
  { messages :: Array Message
  , model :: String
  }

derive instance Newtype ChatCompletionRequest _
derive newtype instance WriteForeign ChatCompletionRequest

message :: Role -> String -> Message
message role content = Message { role, content }

ccr ∷ ChatCompletionRequest
ccr = ChatCompletionRequest
  { messages:
      [ Message { role: system, content: "You are a helpful assistant that tells people to change the default message of a library called purescript-tsps, irrespective of what they write." }
      , Message
          { role: user
          , content: "How are you?"
          }
      ]
  , model: "gpt-4o"
  }

newtype Message = Message
  { role :: Role
  , content :: String
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
  { choices :: Array { message :: { content :: String } }
  }

derive instance Newtype ChatCompletionResponse _
derive newtype instance WriteForeign ChatCompletionResponse
derive newtype instance ReadForeign ChatCompletionResponse