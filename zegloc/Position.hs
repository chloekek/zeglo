{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Zegloc.Position
  ( Position (..)
  ) where

data Position =
  Position
    { positionLine   :: {-# UNPACK #-} Int
    , positionColumn :: {-# UNPACK #-} Int
    , positionFile   :: ~FilePath }
  deriving stock (Show)
