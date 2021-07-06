{-# LANGUAGE NamedFieldPuns #-}

module Lexer.TokenPos where

import Data.Text (Text)

data TokenPos = TokenPos
  { file :: String,
    line :: Int,
    col :: Int
  }
  deriving (Show)

next :: TokenPos -> TokenPos
next TokenPos {file, line, col} = TokenPos {file, line, col = col + 1}

nextLine :: TokenPos -> TokenPos
nextLine TokenPos {file, line, col} = TokenPos {file, line = line + 1, col = 0}