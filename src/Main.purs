module Main where

import Prelude

import Data.Foldable (foldl)
import Data.Traversable (sequence)
import Data.Array as Arr
import Data.Maybe (isNothing, Maybe(..))
import Data.Either (either, Either(..))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)

import Node.Encoding (Encoding(UTF8))
import Node.Process (lookupEnv)
import Node.FS.Sync as FS

import Simple.JSON (writeJSON)

import Github as Github
import Github.Pull as Github.Pull
import Github.Review as Github.Review

toAff :: forall a. Effect a -> Aff a
toAff = liftEffect

readAuth :: Effect (Either Unit Github.BasicAuth)
readAuth = let lookup key = do
                 val <- lookupEnv key
                 pure {key, val}
  in do
    usernameM <- lookup "GH_USERNAME"
    tokenM    <- lookup "GH_TOKEN"
    case [usernameM, tokenM] of
      [{val: Just username}, {val: Just token}] -> pure $ Right $ Github.BasicAuth {username, token}
      fail -> do
        let missings = Arr.filter (\{val} -> isNothing val) fail <#> \{key} -> key
        log $ "env missing: "<>(show missings)
        pure $ Left unit

repo :: Github.RepoId
repo = Github.RepoId {owner: "imercatus", repo: "imercata-backend"}

main :: Effect Unit
main = launchAff_ $ toAff readAuth >>= either (\_ -> pure unit) (doStuff repo)

doStuff :: Github.RepoId -> Github.BasicAuth -> Aff Unit
doStuff repo auth = do
    pullsE <- Github.Pull.get repo auth
    toAff $ case pullsE of
      Right pulls -> (log $ "get pulls OK: "<>(show $ Arr.length pulls)<>" pulls")
      Left  e     -> (log $ "get pulls ERR: "<>(show e))
    either (\_ -> pure unit) (doMoreStuff repo auth) pullsE

doMoreStuff :: Github.RepoId -> Github.BasicAuth -> Array Github.Pull.Pull -> Aff Unit
doMoreStuff repo auth pulls = do
  let effs = pulls <#> \pull -> do
                                  comments <- Github.Review.getComments repo auth pull.number
                                  let logPrefix ="get comments (PR #"<>(show pull.number)<>")"
                                  toAff $ case comments of
                                    Right cmnts -> (log $ logPrefix<>" OK: "<>(show $ Arr.length cmnts)<>" comments")
                                    Left  e     -> (log $ logPrefix<>" ERR: "<>(show e))
                                  pure {pull, comments}
  pullsAndCommentsEs <- sequence effs -- Effect (Array {pull: Pull, comments: Either Err (Array Comment)})
  let pullsAndCommentsE = foldl collectLefts (Right []) pullsAndCommentsEs -- Either (Array Err) {pull: Pull, comments: Array Comment}
  case pullsAndCommentsE of
    Left errs -> toAff $ log $ "errors getting comments: "<>show errs
    Right pullsAndComments -> do
      toAff $ FS.writeTextFile UTF8 "./pulls_and_comments.json" (writeJSON pullsAndComments)
      pure unit
  where
    collectLefts :: Either (Array Github.Err) (Array {pull :: Github.Pull.Pull, comments :: Array Github.Review.Comment})
                 -> {pull :: Github.Pull.Pull, comments :: Either Github.Err (Array Github.Review.Comment)}
                 -> Either (Array Github.Err) (Array {pull :: Github.Pull.Pull, comments :: Array Github.Review.Comment})
    collectLefts (Left errs) {comments: Left err} = Left (errs <> [err])
    collectLefts (Left errs) _ = Left errs
    collectLefts (Right oks) {comments: Left err} = Left [err]
    collectLefts (Right oks) {pull, comments: Right comments} = Right (oks <> [{pull, comments}])
