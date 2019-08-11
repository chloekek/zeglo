{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Zegloc.Name
  ( Identifier (..)
  , Namespace (..)
  , Local (..)
  , Global (..)
  ) where

import Data.Foldable (foldl')
import Data.Hashable (Hashable (..))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Types

data Identifier
  = Word Text
  | Synthesized Word64
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)

newtype Namespace =
  Namespace (Vector Identifier)
  deriving stock (Eq, Ord, Show)

newtype Local =
  Local Identifier
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable)

data Global =
  Global Namespace Identifier
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)

--------------------------------------------------------------------------------
-- Instances

instance Hashable Namespace where
  hashWithSalt salt (Namespace parts) =
    foldl' hashWithSalt salt parts
