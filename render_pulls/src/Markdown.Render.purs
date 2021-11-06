module Markdown.Render where

import Prelude
import Markdown (Document(..), Entry(..), Span, Text(..), Style(..))
import Data.Foldable (foldl)
import Data.Maybe (maybe, Maybe(..))
import Data.Array (head, replicate, findMap)
import Data.String (joinWith, split, Pattern(..))

render :: Document -> String
render (Document _ entries) = entries <#> renderEntry # joinWith "\n\n"

renderEntry :: Entry -> String
renderEntry (Span span)               = renderSpan span
renderEntry (H1 span)                 = "# "  <>renderSpan span
renderEntry (H2 span)                 = "## " <>renderSpan span
renderEntry (H3 span)                 = "### "<>renderSpan span
renderEntry (List entries)            = renderListEntries 0 " - "  entries
renderEntry (OrdList entries)         = renderListEntries 0 " 1. " entries
renderEntry (CodeBlock codeType text) = "```"<>codeType<>"\n"<>text<>"\n```"
renderEntry (Blockquote spans)        =  spans
                                     <#> renderSpan
                                      #  joinWith "\n\n"
                                      #  prefixLinesWith "> "

renderSpan :: Span -> String
renderSpan span = joinWith " " (span <#> renderText)

renderText :: Text -> String
renderText (Text {value, styles}) = let shouldEscape = head styles /= Just Code
                                     in foldl applyStyle value styles

renderListEntries :: Int -> String -> Array Entry -> String
renderListEntries lvl prefix entries = entries <#> renderListEntry lvl prefix # joinWith "\n"

renderListEntry :: Int -> String -> Entry -> String
renderListEntry lvl prefix (List entries)    = renderListEntries (lvl + 1) "- "  entries
renderListEntry lvl prefix (OrdList entries) = renderListEntries (lvl + 1) "1. " entries
renderListEntry lvl prefix ent =
  let indent = (replicate (lvl * 3) " ") # joinWith ""
      bullet = indent <> prefix
  in case ent of
       Span span -> bullet <> renderSpan span
       other     -> bullet <> "not supported as list child: " <> show other

applyStyle :: String -> Style -> String
applyStyle str Bold        = "**"<>str<>"**"
applyStyle str Italic      = "_"<>str<>"_"
applyStyle str Code        = "`"<>str<>"`"
applyStyle str (Link href) = "["<>str<>"]("<>href<>")"

-- Unused
escape :: String -> String
escape str = let escapeCharMaybe char = findMap (\{reserved, escaped} -> if reserved == char then Just escaped else Nothing) reservedChars
                 escapeChar char = maybe char identity (escapeCharMaybe char)
                 replaceWith reserved escaped = {reserved, escaped}
                 reservedChars = [ replaceWith "\\" "&#92;"
                                 , replaceWith "*"  "&#42;"
                                 , replaceWith "_"  "&#95;"
                                 , replaceWith "{"  "&#123;"
                                 , replaceWith "}"  "&#125;"
                                 , replaceWith "["  "&#91;"
                                 , replaceWith "]"  "&#93;"
                                 , replaceWith "#"  "&#35;"
                                 , replaceWith "+"  "&#43;"
                                 , replaceWith "-"  "&#45;"
                                 , replaceWith "."  "&#46;"
                                 , replaceWith "!"  "&#33;"
                                 , replaceWith "`"  "&#96;"
                                 ]
             in split (Pattern "") str <#> escapeChar # joinWith ""

prefixLinesWith :: String -> String -> String
prefixLinesWith prefix str = split (Pattern "\n") str
                         <#> (prefix <> _)
                          #  joinWith "\n"
