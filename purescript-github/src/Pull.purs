module Github.Pull where

import Prelude
import Milkis as M
import Data.Maybe (Maybe)
import Data.Array as Arr
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

import Github (BasicAuth, basicAuthValue, RepoId(..), Err, Rep, _getRep, User)

import Simple.JSON as Json
import Effect.Http as Http

type Repo = {name :: String, full_name :: String}
type Ref = {repo :: Repo, ref :: String, sha :: String, user :: User}
type Pull = { url        :: String
            , number     :: Number
            , state      :: String
            , title      :: String
            , body       :: Maybe String
            , created_at :: String
            , updated_at :: String
            , closed_at  :: Maybe String
            , merged_at  :: Maybe String
            , user       :: User
            , base       :: Ref
            , head       :: Ref
            }

get :: RepoId -> BasicAuth -> Aff.Aff(Rep (Array Pull))
get repo key = getPulls_ [] 1 repo key
  where
  perPage = 100
  query page = unsafeCoerce {state: "all", per_page: perPage, page}
  buildUrl page (RepoId {owner, repo}) = "https://api.github.com/repos/"<>owner<>"/"<>repo<>"/pulls?"<>(toQueryString $ query page)
  url page repo = (M.URL $ buildUrl page repo)
  options auth = do
    authValue <- basicAuthValue auth
    pure { method: M.getMethod
         , headers: M.makeHeaders { "Accept": "application/vnd.github.v3+json"
                                  , "Authorization": "Basic "<>authValue
                                  }
         }
  getPulls_ prev page repo auth = do
    opts <- liftEffect $ options auth
    rawRep <- Aff.attempt $ Http.fetch (url page repo) opts
    rep <- _getRep rawRep
    case rep of
      Right pulls -> if Arr.length pulls == perPage then
                       getPulls_ (prev <> pulls) (page + 1) repo auth
                     else
                       pure $ Right $ prev <> pulls
      left        -> pure left
