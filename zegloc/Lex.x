{
{-# LANGUAGE OverloadedStrings #-}

module Zegloc.Lex where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
}

%wrapper "basic-bytestring"

tokens :-

  "end"                       { keyword }
  "forall"                    { keyword }
  "is"                        { keyword }
  "my"                        { keyword }
  "par"                       { keyword }
  "signature"                 { keyword }
  "using"                     { keyword }
  "value"                     { keyword }
  "yield"                     { keyword }
  "zeglo"                     { keyword }

  [A-Za-z][A-Za-z0-9\-]*      { word }

{
data Token
  = Word Text
  | Keyword Text

word :: ByteString -> Token
word = Keyword . toStrict . decodeUtf8

keyword :: ByteString -> Token
keyword = Keyword . toStrict . decodeUtf8
}
