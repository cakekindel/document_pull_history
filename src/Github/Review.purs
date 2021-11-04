module Github.Review where

import Prelude
import Milkis as M
import Data.Array as Arr
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

import Github (BasicAuth, basicAuthValue, RepoId(..), Err(..), Rep, _getRep, User)

import Simple.JSON as Json
import Effect.Http as Http

type Comment = { pull_request_review_id :: Number
               , id :: Number
               , diff_hunk :: Maybe String
               , path :: String
               , in_reply_to_id :: Maybe Number
               , user :: User
               , body :: String
               , created_at :: String
               }

getComments :: RepoId -> BasicAuth -> Number -> Rep (Array Comment)
getComments repo key pr = get_ [] 1 repo pr key
  where
  perPage = 30
  query page = unsafeCoerce {per_page: perPage, page}
  buildUrl page (RepoId {owner, repo}) = "https://api.github.com/repos/"<>owner<>"/"<>repo<>"/pulls/"<>(show pr)<>"/comments?"<>(toQueryString $ query page)
  url page repo = (M.URL $ buildUrl page repo)
  options auth = do
    authValue <- basicAuthValue auth
    pure { method: M.getMethod
         , headers: M.makeHeaders { "Accept": "application/vnd.github.v3+json"
                                  , "Authorization": "Basic "<>authValue
                                  }
         }
  get_ prev page repo pr auth = do
    opts <- liftEffect $ options auth
    rawRep <- Aff.attempt $ Http.fetch (url page repo) opts
    rep <- _getRep rawRep
    case rep of
      Right pulls -> if Arr.length pulls == perPage then
                       get_ (prev <> pulls) (page + 1) repo pr auth
                     else
                       pure $ Right $ prev <> pulls
      left        -> pure left
