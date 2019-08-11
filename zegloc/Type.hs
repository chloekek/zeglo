{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

module Zegloc.Type
  ( -- * Types
    Type (..)
  , pattern (:->:)

    -- * Constraints
  , Constraint (..)

    -- * Identifiers
  , Unknown (..)
  , Skolem (..)
  ) where

import Data.Word (Word64)

import Zegloc.Name (Local)

--------------------------------------------------------------------------------
-- Types

data Type

  -- |
  -- The inference program returns the error type when an expression is
  -- ill-typed. This allows inference to continue to collect more errors, rather
  -- than aborting immediately.
  = ErrorT

  | UnknownT Unknown

  | LocalT Local

  -- |
  -- Apply a type constructor (the first field) to a type (the second field).
  | ApplyT Type Type

  -- |
  -- Universally quantified type.
  | ForallT Local Type

  -- |
  -- Constrained type.
  | ConstrainedT Constraint Type

  -- |
  -- The type constructor for functions. To create or match an actual function
  -- type, use the ':->:' pattern synonym.
  | FunctionT

  deriving stock (Show)

pattern (:->:) :: Type -> Type -> Type
pattern (:->:) a b = ApplyT (ApplyT FunctionT a) b

--------------------------------------------------------------------------------
-- Constraints

data Constraint
  = EqualityC Type Type
  | TypeClassC
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Identifiers

newtype Unknown =
  Unknown Word64
  deriving stock (Show)

newtype Skolem =
  Skolem Word64
  deriving stock (Show)
