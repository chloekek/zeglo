{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

-- |
-- ANF is the internal representation used for type-checked programs. Evidences
-- are explicit. ANF is untyped.
module Zegloc.Anf
  ( -- * Expressions
    Anf (..)
  , Expression (..)
  , Value (..)

    -- * Evidence placeholders
  , EvidencePlaceholder (..)
  ) where

import Data.Vector (Vector)
import Data.Word (Word64)

import Zegloc.Name (Global, Local)

--------------------------------------------------------------------------------
-- Expressions

data Anf =
  Anf
    { anfBindings :: Vector (Local, Expression)
    , anfResult   :: Value }
  deriving stock (Show)

data Expression

  -- |
  -- Evaluate to the return value of a function applied to an argument.
  = ApplyE Value Value

  -- |
  -- Evaluate to a function that has a new parameter in its lexical scope.
  | LambdaE Local Anf

  -- |
  -- See 'EvidencePlaceholder'.
  | EvidencePlaceholderE EvidencePlaceholder

  deriving stock (Show)

data Value
  -- |
  -- The elaboration program returns the error value when an expression is
  -- ill-typed. This allows inference to continue to collect more errors, rather
  -- than aborting immediately.
  = ErrorV

  | LocalV Local
  | GlobalV Global

  deriving stock (Show)

--------------------------------------------------------------------------------
-- Evidence placeholders

-- |
-- An evidence placeholder is inserted during elaboration for every constraint
-- at every place a polymorphic variable is referenced. After solving the
-- constraints, the placeholders are replaced by expressions that evaluate to
-- the evidence at runtime. This substitution process is known as zonking,
-- because that is what GHC calls it.
newtype EvidencePlaceholder =
  EvidencePlaceholder Word64
  deriving stock (Show)
