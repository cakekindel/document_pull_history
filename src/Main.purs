module Main where

import Prelude

import Partial.Unsafe (unsafePartial)

import Data.Foldable (foldl)
import Data.String as S
import Data.Traversable (sequence)
import Data.Array as Arr
import Data.Array ((!!))
import Data.Maybe (maybe, fromJust, isNothing, Maybe(..))
import Data.Either (Either(..))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)

import Node.Encoding (Encoding(UTF8))
import Node.Process (lookupEnv)
import Node.FS.Sync as FS
import Node.Path (FilePath)

import Simple.JSON (writeJSON)

import Github as Github
import Github.Pull as Github.Pull
import Github.Review as Github.Review

toAff :: forall a. Effect a -> Aff a
toAff = liftEffect

type Env = { repo    :: Github.RepoId    -- REPO:                   owner/repo e.g. cakekindel/mergebot
           , auth    :: Github.BasicAuth -- GH_USERNAME / GH_TOKEN: credentials
           , outFile :: FilePath         -- OUT_FILE (optional):    file to write responses to
           }

readEnv :: Effect (Either Unit Env)
readEnv = let
    lookup key = lookupEnv key <#> \val -> {key, val}
    parseRepo repo = repo
                   # S.split (S.Pattern "/")
                   # \splat -> do
                                 owner <- splat !! 0
                                 repo  <- splat !! 1
                                 Just $ Github.RepoId {owner, repo}
                   # unsafePartial fromJust
  in do
    repoM     <- lookup "REPO"
    usernameM <- lookup "GH_USERNAME"
    tokenM    <- lookup "GH_TOKEN"
    outFileM  <- lookup "OUT_FILE"

    case [repoM, usernameM, tokenM] of
      [ {val: Just repo}
      , {val: Just username}
      , {val: Just token}
      ]   -> pure
            $ Right
            $ { repo: parseRepo repo
              , auth: Github.BasicAuth {username, token}
              , outFile: maybe "./out.json" identity outFileM.val
              }
      fail -> do
        let missings = Arr.filter (\{val} -> isNothing val) fail <#> \{key} -> key
        log $ "env missing: "<>(show missings)
        pure $ Left unit

main :: Effect Unit
main = launchAff_ do
  env' <- toAff readEnv
  case env' of
    Right env -> doStuff env.outFile env.repo env.auth
    Left  _   -> pure unit

type PullAnd c = {pull :: Github.Pull.Pull, comments :: c}
type PullAndComments = PullAnd (Array Github.Review.Comment)

doStuff :: FilePath -> Github.RepoId -> Github.BasicAuth -> Aff Unit
doStuff outFile repo auth = do
    pullsE <- Github.Pull.get repo auth
    case pullsE of
      Left  e     -> toAff $ log $ "get pulls ERR: "<>(show e)
      Right pulls -> do
        pullsAndCommentsE <- addComments pulls
        toAff $ log $ "get pulls OK: "<>(show $ Arr.length pulls)<>" pulls"

        case pullsAndCommentsE of
          Left  errs             -> toAff $ log $ "errors getting comments: "<>show errs
          Right pullsAndComments -> do
            toAff $ FS.writeTextFile UTF8 outFile (writeJSON pullsAndComments)
            toAff $ log $ "wrote "<>(show $ Arr.length pulls)<>" entries to "<>outFile<>"."
            pure unit
  where
    addComments :: Array Github.Pull.Pull
                -> Aff (Either (Array Github.Err) (Array PullAndComments))
    addComments pulls = do
      let effs = pulls <#> getAndBindComments
      pullsAndCommentsEs <- sequence effs
      pure $ foldl collectLefts (Right []) pullsAndCommentsEs

    getAndBindComments :: Github.Pull.Pull
                       -> Aff (PullAnd (Github.Rep (Array Github.Review.Comment)))
    getAndBindComments pull = do
      comments <- Github.Review.getComments repo auth pull.number
      let logPrefix = "get comments (PR #"<>(show pull.number)<>")"
      toAff $ case comments of
        Right cmnts -> (log $ logPrefix<>" OK: "<>(show $ Arr.length cmnts)<>" comments")
        Left  e     -> (log $ logPrefix<>" ERR: "<>(show e))
      pure {pull, comments}

    collectLefts :: Either (Array Github.Err) (Array PullAndComments)
                 -> PullAnd (Github.Rep (Array Github.Review.Comment))
                 -> Either (Array Github.Err) (Array PullAndComments)
    collectLefts (Left errs) {comments: Left err} = Left (errs <> [err])
    collectLefts (Left errs) _                    = Left errs
    collectLefts (Right oks) {comments: Left err} = Left [err]
    collectLefts (Right oks) {pull, comments: Right comments} = Right (oks <> [{pull, comments}])
