module Markdown where

import Prelude
import Data.Array ((:), singleton)

type CodeType = String
data Document = Document String (Array Entry)

type Span = Array Text

data Entry = H1 Span
           | H2 Span
           | H3 Span
           | H4 Span
           | Span Span
           | List (Array Entry)
           | OrdList (Array Entry)
           | Blockquote (Array Span)
           | CodeBlock CodeType String
           | Collapsible String (Array Entry)
           | Raw String

instance showEntry :: Show Entry where
  show (Raw text)         = "Raw " <> show text
  show (H1 span)          = "H1 " <> show span
  show (H2 span)          = "H2 " <> show span
  show (H3 span)          = "H3 " <> show span
  show (H4 span)          = "H4 " <> show span
  show (Span span)        = "Span " <> show span
  show (List entries)     = "List " <> show entries
  show (OrdList entries)  = "OrdList " <> show entries
  show (Blockquote spans) = "Blockquote " <> show spans
  show (CodeBlock ty val) = "CodeBlock " <> show ty <> " " <> show val
  show (Collapsible summary entries) = "Collapsible " <> show summary <> " " <> show entries

instance showText :: Show Text where
  show (Text record) = "Text " <> show record

instance showStyle :: Show Style where
  show Bold = "Bold"
  show Italic = "Italic"
  show Code = "Code"
  show (Link href) = "Link " <> show href

data Text = Text {value :: String, styles :: Array Style}
data Style = Bold
           | Italic
           | Code
           | Link String

instance eqStyle :: Eq Style where
  eq Bold Bold = true
  eq Italic Italic = true
  eq Code Code = true
  eq (Link hrefA) (Link hrefB) | hrefA == hrefB = true
  eq _ _ = false

md :: String -> Array Entry -> Document
md name entries = Document name entries

raw :: String -> Entry
raw = Raw

h1 :: Span -> Entry
h1 = H1

collapsible :: String -> Array Entry -> Entry
collapsible = Collapsible

hr :: Entry
hr = raw "---"

h2 :: Span -> Entry
h2 = H2

h3 :: Span -> Entry
h3 = H3

h4 :: Span -> Entry
h4 = H4

blockquote :: Array Span -> Entry
blockquote = Blockquote

codeBlock :: CodeType -> String -> Entry
codeBlock ty contents = CodeBlock ty contents

ul :: Array Entry -> Entry
ul = List

ol :: Array Entry -> Entry
ol = OrdList

text :: String -> Text
text value = Text {value, styles: []}

textStyle :: Style -> Text -> Text
textStyle style (Text {value, styles}) = Text {value, styles: style : styles}

link :: String -> Text -> Text
link href = textStyle (Link href)

bold :: Text -> Text
bold = textStyle Bold

italic :: Text -> Text
italic = textStyle Italic

codeInline :: Text -> Text
codeInline = textStyle Code

span :: Span -> Entry
span = Span

span_ :: Text -> Entry
span_ = singleton >>> span
