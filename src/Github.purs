module Github where

import Prelude
import Milkis as M
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Aff as Aff
import Foreign (MultipleErrors)
import Unsafe.Coerce (unsafeCoerce)
import Node.URL (Query, toQueryString)
import Node.Buffer as Buf
import Node.Encoding (Encoding(UTF8, Base64))
import Data.TextEncoder (encodeUtf8)
import Data.Bifunctor (lmap)

import Simple.JSON as Json
import Effect.Http as Http

newtype BasicAuth = BasicAuth {username :: String, token :: String}
basicAuthValue :: BasicAuth -> Effect String
basicAuthValue (BasicAuth {username, token}) = encode $ username<>":"<>token
  where
    toBuf :: String -> Effect Buf.Buffer
    toBuf s = Buf.fromString s UTF8
    toB64 :: Buf.Buffer -> Effect String
    toB64 = Buf.toString Base64
    encode s = toBuf s >>= toB64

newtype RepoId = RepoId {owner :: String, repo :: String}

data Err = JsonErr MultipleErrors
         | HttpErr Error
         | HttpStatusErr Int

instance showErr :: Show Err where
  show (JsonErr errs)       = "JsonErr "<>Json.unsafeStringify errs
  show (HttpErr _)          = "HttpErr _"
  show (HttpStatusErr code) = "HttpStatusErr "<>show code

type Rep ok = Either Err ok

type User = { login :: String, html_url :: String }

_getRep :: forall t. Json.ReadForeign t => Either Error M.Response -> Aff.Aff (Rep t)
_getRep (Left e)    = pure $ Left (HttpErr e)
_getRep (Right rep) = do
                        let code = M.statusCode rep
                        case code of
                          200 -> do
                            jsonRaw <- M.json rep
                            Json.read jsonRaw # lmap JsonErr # pure
                          notOk -> pure $ Left (HttpStatusErr notOk)
