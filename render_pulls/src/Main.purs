module Main where

import Prelude

import Partial.Unsafe (unsafePartial)

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console (log)
import Effect.Exception (error)

import Simple.JSON (readJSON_)
import JSURI (encodeURIComponent)

import Node.Process (lookupEnv)
import Node.Path as Path
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS

import Control.Monad.Error.Class (throwError)

import Data.String (replace, Pattern(..), Replacement(..))
import Data.String.Padding (padStart)
import Data.Either (fromRight, Either(..))
import Data.Maybe (isJust, optional, fromJust, maybe, Maybe(..), isNothing)
import Data.Ord (compare)
import Data.Foldable (foldl)
import Data.List (List(Nil))
import Data.Array (concat, sortBy, singleton, filter, catMaybes)
import Data.Array.NonEmpty as NonEmpty
import Data.Int (floor)
import Data.Traversable (sequence)
import Data.JSDate as Date
import Data.Formatter.DateTime (parseFormatString, format)

import Github.Pull as Github.Pull
import Github.Review as Github.Review

import Markdown as MD
import Markdown.Render (render)

type Env = { inFile :: Path.FilePath -- FILE:    relpath to output of get_pulls
           , outDir :: Path.FilePath -- OUT_DIR: relpath to directory to write documents to
           }

readEnv :: Effect Env
readEnv = let
    lookup key = lookupEnv key <#> \val -> {key, val}
  in do
    inFileM  <- lookup "FILE"
    outDirM <- lookup "OUT_DIR"

    case [inFileM, outDirM] of
      [ {val: Just inFile}, {val: Just outDir} ] -> pure { inFile, outDir }
      fail -> do
        let missings = filter (\{val} -> isNothing val) fail <#> \{key} -> key
        let msg = "env missing: "<>(show missings)
        log msg
        throwError (error msg)

type PullAndComments = {pull :: Github.Pull.Pull, comments :: Array Github.Review.Comment}

main :: Effect Unit
main = do
  {inFile, outDir} <- readEnv
  jsonContents <- FS.readTextFile UTF8 inFile
  let pulls = readJSON_ jsonContents :: Maybe (Array PullAndComments)

  case pulls of
    Nothing -> do
      log "Failed to parse pulls"
      pure unit
    Just pulls' -> do
      _ <-  pulls'
        <#> pullDocument
        <#> writeDocument outDir
         #  sequence
      pure unit

pullDocument :: PullAndComments -> MD.Document
pullDocument pc@{pull, comments} =
  let filename = "pr_"<>(pullNumber # padStart 5 '0')
      pullNumber = pull.number # floor # show
  in  MD.md filename
    $ catMaybes
    $ [ Just $ MD.h1 ["Pull Request #"<>(pull.number # floor # show)<>": "<>pull.title # MD.text] ]
      <> pullDocBody pc
      <> pullDocDiff pc
      <> pullDocConversation pc

pullDocBody :: PullAndComments -> Array (Maybe MD.Entry)
pullDocBody {pull, comments} =  pull.body
                            <#> (\body -> [ MD.h2 ["Description" # MD.text]
                                          , MD.raw body
                                          ])
                             #  maybe [] identity
                            <#> Just

pullDocDiff :: PullAndComments -> Array (Maybe MD.Entry)
pullDocDiff {pull, comments} =
  let
    diffHref = "https://github.com/"
            <> "cakekindel/"
            <> pull.head.repo.name
            <> "/search?q="
            <> (pull.title # encodeURIComponent # (unsafePartial fromJust))
            <> "&type=commits"
  in
   sequence
   $ Just
   $ [ MD.h2 ["Changes" # MD.text]
     , MD.span ["Open diff" # MD.text # MD.link diffHref]
     ]

pullDocConversation :: PullAndComments -> Array (Maybe MD.Entry)
pullDocConversation {pull, comments} =
  let
    dateFormatter = fromRight Nil $ parseFormatString "DD/MM/YYYY hh:mm"
    renderDate comment = unsafePerformEffect (Date.parse comment.created_at)
                       # Date.toDateTime
                       # unsafePartial fromJust
                       # format dateFormatter
    children comments comment = NonEmpty.filter (\c -> c.in_reply_to_id == Just comment.id) comments
                              # sortBy (\a b -> unsafePerformEffect do
                                                  aDate <- Date.parse a.created_at
                                                  bDate <- Date.parse b.created_at
                                                  pure $ compare aDate bDate
                                       )
    renderComment comment =
      let notReply = if isJust comment.in_reply_to_id then Nothing else Just unit
       in catMaybes
          $ [ Just $ MD.h4 [("@"<>comment.user.login) # MD.text]
            , Just $ MD.span [renderDate comment # MD.text]
            , notReply <#> (const $ MD.span [comment.path # MD.text # MD.codeInline])
            , notReply >>= const comment.diff_hunk <#> \diff -> MD.collapsible "Click to expand highlighted changes" [MD.codeBlock "diff" diff]
            , Just $ MD.raw comment.body
            ]
    reduceComment  comments md comment = case comment.in_reply_to_id of
                                           Just _  -> md
                                           Nothing -> md
                                                   <> renderComment comment
                                                   <> concat (children comments comment <#> renderComment)
                                                   <> [MD.hr]
    renderComments comments = foldl (reduceComment comments) [] comments
  in NonEmpty.fromArray comments
     <#> (\comments' -> [ MD.h2 ["Conversation" # MD.text] ] <> renderComments comments')
      #  maybe [] identity
     <#> Just

writeDocument :: Path.FilePath -> MD.Document -> Effect Unit
writeDocument dir doc@(MD.Document name _) =
  let path = Path.concat [dir, name <> ".md"]
  in FS.writeTextFile UTF8 path (render doc)
