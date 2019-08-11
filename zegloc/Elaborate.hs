{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- |
-- The elaboration code is responsible for generating ANF, ensuring that the
-- program is well-typed, and inserting evidences in the correct places.
--
-- This module exposes the low-level elaboration EDSL that upholds the
-- aforementioned guarantees.
module Zegloc.Elaborate
  ( -- * Types
    Elaborate
  , Error (..)
  , ErrorPayload (..)
  , runElaborate

    -- * Primitives
  , elabLocal
  , elabGlobal
  , elabApply
  , elabLambda

    -- * Positions
  , setPosition
  , resetPosition
  ) where

import Control.Lens (Lens', (&), (^.), (%=), (.=), (?=), (<>=), (<<+=), (%~), (?~), _1, at, lens, sans, use, uses)
import Control.Monad (foldM)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.State (StateT, runState)
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Data.DList (DList)
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Word (Word64)

import qualified Control.Monad.Writer.Class as Writer
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

import Zegloc.Anf (Anf (..), EvidencePlaceholder (..), Expression (..), Value (..))
import Zegloc.Name (Global, Identifier (..), Local (..))
import Zegloc.Position (Position)
import Zegloc.Type (pattern (:->:), Constraint (..), Skolem (..), Type (..), Unknown (..))

--------------------------------------------------------------------------------
-- Types

-- |
-- An elaboration action interacts with the elaboration environment. The
-- following capabilities are provided:
--
--  - Looking up the types of local variables.
--  - Looking up the types of global variables.
--  - Constructing an ANF expression.
--  - Generating new unknowns and Skolems.
--  - Collecting constraints.
--  - Collecting error messages.
--  - Tracking source positions.
--
-- Running an action takes care of solving constraints and updating the
-- generated ANF expression with resolved evidences.
newtype Elaborate a =
  Elaborate (StateT State Identity a)
  deriving newtype (Applicative, Functor, Monad)

-- |
-- The elaboration state contains all the necessary information for building up
-- ANF expressions.
data State =
  State
    { -- WARNING: WHEN YOU ADD NEW FIELDS HERE, MAKE SURE THAT YOU ALSO UPDATE
      -- WARNING: THEM IN elabLambda IF NECESSARY. OTHERWISE THEY WILL NOT BE
      -- WARNING: UPDATED AFTER CHECKING THE LAMBDA BODY.

      -- |
      -- The next unique identifier to generate. Used for unknowns and Skolems.
      stateNextId :: Word64

      -- |
      -- The current position in the source code that we are elaborating for.
      -- This is used for pointing at the culprit in error messages.
    , statePosition :: Maybe Position

      -- |
      -- The errors that we have collected thus far. A non-empty list always
      -- indicates an ill-typed program.
    , stateErrors :: [Error]

      -- |
      -- The constraints that we have collected thus far. If an evidence
      -- placeholder is present, then it will be located in the ANF program and
      -- replaced by the evidence for the solution to the constraint.
    , stateConstraints :: [(Constraint, Maybe EvidencePlaceholder)]

      -- |
      -- The sequence of let bindings that we are generating in this
      -- elaboration.
    , stateBindings :: DList (Local, Expression)

      -- |
      -- The type for each local that is in scope.
    , stateLocals :: HashMap Local Type

      -- |
      -- The type for each known global.
    , stateGlobals :: HashMap Global Type }

  deriving stock (Show)

-- |
-- Part of the elaboration procedure is type checking, and therefore it may
-- fail.
data Error =
  Error
    { errorPosition :: Maybe Position
    , errorPayload :: ErrorPayload }
  deriving stock (Show)

data ErrorPayload
  = UndefinedLocal Local
  | UndefinedGlobal Global
  deriving stock (Show)

runElaborate
  :: Elaborate (Value, a)
  -> HashMap Global Type
  -> Either (NonEmpty Error) (Anf, a)
runElaborate (Elaborate action) globals =
  let
    state = State { stateNextId      = 0
                  , statePosition    = Nothing
                  , stateErrors      = []
                  , stateConstraints = []
                  , stateBindings    = []
                  , stateLocals      = HashMap.empty
                  , stateGlobals     = globals }
    ((result, t), state') = runState action state
    bindings = state' ^. _stateBindings & Vector.fromList . DList.toList
    -- TODO: Solve constraints.
    -- TODO: Zonk unknowns and placeholders.
  in
    case state' ^. _stateErrors of
      []       -> Right (Anf bindings result, t)
      (e : es) -> Left  (e :| es)

--------------------------------------------------------------------------------
-- Primitives

-- |
-- Generate a new binding with a fresh name. The given type and expression are
-- used verbatim and not examined.
bind :: MonadState State m => (Expression, a) -> m (Value, a)
bind (expr, t) = do
  name <- freshLocal
  _stateBindings <>= [(name, expr)]
  pure (LocalV name, t)

elabLocal :: Local -> Elaborate (Value, Type)
elabLocal name = Elaborate $

  -- Look up the name in the current scope and hope it is there.
  use (_stateLocals . at name) >>= \case

    Nothing -> do
      -- We did not find a local with this name in the current scope. This means
      -- that the program is ill-formed, so we raise an error.
      recordError $ UndefinedLocal name
      pure (ErrorV, ErrorT)

    Just type_ -> do
      -- We found this name in the current scope, hurray! What we have to do now
      -- is instantiate the type and elaborate the expression. Instantiating the
      -- type replaces foralls by unknowns, allowing this expression to be used
      -- with different types if it is polymorphic. Elaboration applies the
      -- elaborations that result from the instantiation, ensuring that type
      -- arguments and type class dictionaries are explicitly provided.
      (elabs, type') <- instantiate type_
      let value = (LocalV name, type')
      foldM applyElaboration value elabs

elabGlobal :: Global -> Elaborate (Value, Type)
elabGlobal name = Elaborate $
  -- This works just like elabLocal, so go and check out that function to learn
  -- more about how this works.
  use (_stateGlobals . at name) >>= \case
    Nothing -> do
      recordError $ UndefinedGlobal name
      pure (ErrorV, ErrorT)
    Just type_ -> do
      (elabs, type') <- instantiate type_
      let value = (GlobalV name, type')
      foldM applyElaboration value elabs

elabApply :: (Value, Type) -> (Value, Type) -> Elaborate (Value, Type)
elabApply (func, funcType) (arg, argType) = Elaborate $ do
  resultType <- UnknownT <$> freshUnknown
  recordConstraint (EqualityC funcType (argType :->: resultType)) Nothing
  bind (ApplyE func arg, resultType)

elabLambda :: Local -> Elaborate (Value, Type) -> Elaborate (Value, Type)
elabLambda param (Elaborate body) = Elaborate $ do
  -- First we generate a new type for the parameter and we add it to the local
  -- scope so that the body can access it.
  paramType <- UnknownT <$> freshUnknown
  bodyState <- uses id $ _stateLocals . at param ?~ paramType

  -- We then elaborate the body, giving us a new state and a result value. The
  -- reason we run this under a separate state is that we do not want this to
  -- update our state, except for some fields which we update manually.
  let ((result, resultType), bodyState') = runState body bodyState
  _stateNextId       .= bodyState' ^. _stateNextId
  _statePosition     .= bodyState' ^. _statePosition
  _stateErrors      <>= bodyState' ^. _stateErrors
  _stateConstraints <>= bodyState' ^. _stateConstraints

  -- And then we construct the lambda expression.
  let bindings = bodyState' ^. _stateBindings & Vector.fromList . DList.toList
  bind ( LambdaE param (Anf bindings result)
       , paramType :->: resultType )

--------------------------------------------------------------------------------
-- Elaboration and inference

-- |
-- An elaboration is an extra argument that is inserted into the expression when
-- elaborating it. Elaborations result from instantiation.
newtype Elaboration

  -- |
  -- Apply the expression to a constraint solution evidence. We do not know at
  -- this point what the evidence is, since that information is not available
  -- until after constraint solving, so we apply it to a placeholder.
  = ElabEvidence EvidencePlaceholder

  deriving stock (Show)

-- |
-- Apply an expression to an elaboration.
applyElaboration :: MonadState State m => (Value, a) -> Elaboration -> m (Value, a)

applyElaboration (value, t) (ElabEvidence evidence) = do
  (evidenceV, ()) <- bind (EvidencePlaceholderE evidence, ())
  bind (ApplyE value evidenceV, t)

-- |
-- Replace leading foralls with fresh unknowns, remove constraints, and generate
-- elaborations.
instantiate :: forall m. MonadState State m => Type -> m ([Elaboration], Type)
instantiate = fmap ((_1 %~ DList.toList) . swap) . runWriterT .
                go1 HashMap.empty
  where
  -- Instantiation happens in two phases:
  --
  --  1. Generate elaborations for and instantiate foralls and constraints at
  --     the front of the type.
  --  2. Substitute instantiations for local type variables in the rest of the
  --     type.

  go1 :: HashMap Local Type -> Type -> WriterT (DList Elaboration) m Type

  go1 e (ForallT t u) = do
    -- For a universally quantified type, we generate a new unknown for the type
    -- variable and add it to the substitutions.
    t' <- UnknownT <$> freshUnknown
    go1 (e & at t ?~ t') u

  go1 e (ConstrainedT c t) = do
    -- For a constrained type we record the constraint, insert an evidence
    -- placeholder, and record an evidence elaboration.
    evidencePlaceholder <- freshEvidencePlaceholder
    recordConstraint c (Just evidencePlaceholder)
    Writer.tell [ElabEvidence evidencePlaceholder]

    go1 e t

  go1 e other =
    -- For anything else we advance to the next phase.
    go2 e other

  go2 :: HashMap Local Type -> Type -> WriterT (DList Elaboration) m Type
  go2 _ t@ErrorT           = pure t
  go2 _ t@UnknownT{}       = pure t
  go2 e t@(LocalT u)       = pure $ fromMaybe t (e ^. at u)
  go2 e (ApplyT t u)       = ApplyT <$> go2 e t <*> go2 e u
  go2 e (ForallT t u)      = ForallT t <$> go2 (e & sans t) u
  go2 e (ConstrainedT c t) = ConstrainedT <$> go2C e c <*> go2 e t
  go2 _ t@FunctionT        = pure t

  go2C :: HashMap Local Type -> Constraint -> WriterT (DList Elaboration) m Constraint
  go2C e (EqualityC t u) = EqualityC <$> go2 e t <*> go2 e u
  go2C _ c@TypeClassC    = pure c

--------------------------------------------------------------------------------
-- Errors

recordError :: MonadState State m => ErrorPayload -> m ()
recordError payload = do
  position <- use _statePosition
  _stateErrors %= (Error position payload :)

--------------------------------------------------------------------------------
-- Constraints

recordConstraint :: MonadState State m => Constraint -> Maybe EvidencePlaceholder -> m ()
recordConstraint = curry $ (_stateConstraints %=) . (:)

--------------------------------------------------------------------------------
-- Generating fresh objects

freshId :: MonadState State m => m Word64
freshId = _stateNextId <<+= 1

freshLocal :: MonadState State m => m Local
freshLocal = Local . Synthesized <$> freshId

freshUnknown :: MonadState State m => m Unknown
freshUnknown = Unknown <$> freshId

freshSkolem :: MonadState State m => m Skolem
freshSkolem = Skolem <$> freshId

freshEvidencePlaceholder :: MonadState State m => m EvidencePlaceholder
freshEvidencePlaceholder = EvidencePlaceholder <$> freshId

--------------------------------------------------------------------------------
-- Positions

setPosition :: Position -> Elaborate ()
setPosition = Elaborate . (_statePosition ?=)

resetPosition :: Elaborate ()
resetPosition = Elaborate $ _statePosition .= Nothing

--------------------------------------------------------------------------------
-- Optics

_stateNextId :: Lens' State Word64
_stateNextId = lens stateNextId (\s a -> s { stateNextId = a })

_statePosition :: Lens' State (Maybe Position)
_statePosition = lens statePosition (\s a -> s { statePosition = a })

_stateErrors :: Lens' State [Error]
_stateErrors = lens stateErrors (\s a -> s { stateErrors = a })

_stateConstraints :: Lens' State [(Constraint, Maybe EvidencePlaceholder)]
_stateConstraints = lens stateConstraints (\s a -> s { stateConstraints = a })

_stateBindings :: Lens' State (DList (Local, Expression))
_stateBindings = lens stateBindings (\s a -> s { stateBindings = a })

_stateLocals :: Lens' State (HashMap Local Type)
_stateLocals = lens stateLocals (\s a -> s { stateLocals = a })

_stateGlobals :: Lens' State (HashMap Global Type)
_stateGlobals = lens stateGlobals (\s a -> s { stateGlobals = a })
