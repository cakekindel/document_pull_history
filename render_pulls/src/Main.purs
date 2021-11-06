module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error)

import Simple.JSON (readJSON_)

import Node.Process (lookupEnv)
import Node.Path (FilePath, concat)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS

import Data.String (replace, Pattern(..), Replacement(..))
import Data.Either (Either(..))
import Data.Maybe (maybe, Maybe(..), isNothing)
import Data.Array (singleton, filter)
import Data.Int (floor)
import Data.Traversable (sequence)
import Control.Monad.Error.Class (throwError)

import Github.Pull as Github.Pull
import Github.Review as Github.Review

import Markdown as MD
import Markdown.Render (render)

type Env = { inFile :: FilePath -- FILE:    relpath to output of get_pulls
           , outDir :: FilePath -- OUT_DIR: relpath to directory to write documents to
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
pullDocument {pull, comments} = MD.md ("pr_"<>(show $ floor pull.number))
                                      [ MD.h1 [pull.title # MD.text]
                                      , MD.span [pull.body <#> MD.text # maybe ("No description" # MD.text # MD.italic) identity]
                                      ]

writeDocument :: FilePath -> MD.Document -> Effect Unit
writeDocument dir doc@(MD.Document name _) =
  let path = concat [dir, name <> ".md"]
  in FS.writeTextFile UTF8 path (render doc)
