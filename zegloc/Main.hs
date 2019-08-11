{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Zegloc.Main
  ( main
  ) where

import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Data.HashMap.Strict (HashMap)

import Zegloc.Elaborate

import Zegloc.Anf (Value)
import Zegloc.Anf.Pretty (prettyAnf)
import Zegloc.Lex ()
import Zegloc.Name (Global (..), Identifier (..), Local (..), Namespace (..))
import Zegloc.Type (pattern (:->:), Constraint (..), Type (..))

main :: IO ()
main =
  let
    f, g    :: Global
    a, x, y :: Local
    f = Global (Namespace []) (Word "f")
    g = Global (Namespace []) (Word "g")
    a = Local (Word "a")
    x = Local (Word "x")
    y = Local (Word "y")

    globals :: HashMap Global Type
    globals =
      [ (f, ForallT a (LocalT a :->: LocalT a))
      , (g, ForallT a (ConstrainedT TypeClassC (LocalT a :->: LocalT a))) ]

    elaborate :: Elaborate (Value, Type)
    elaborate =
      elabLambda x $
        elabLambda y $ do
          f'  <- elabGlobal f
          x'  <- elabLocal x
          y'  <- elabLocal y
          fx  <- elabApply f' x'
          fxy <- elabApply fx y'

          g' <- elabGlobal g
          gfxy <- elabApply g' fxy

          pure gfxy

  in
    case runElaborate elaborate globals of
      Left err -> fail (show err)
      Right (ok, _) -> putDoc (prettyAnf ok)
