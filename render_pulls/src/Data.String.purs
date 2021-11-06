module Data.String.Padding where

import Prelude

import Data.String as S
import Data.String.CodeUnits (singleton)
import Data.Array as A

padStart :: Int -> Char -> String -> String
padStart len withChar str =
  let with = singleton withChar
      go str | S.length str >= len = str
      go str = go (with<>str)
  in go str
