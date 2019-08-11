module Zegloc.Anf.Pretty
  ( prettyAnf
  , prettyExpression
  , prettyValue
  ) where

import Data.Foldable (fold)
import Data.Function ((&))
import Data.List (intersperse)
import Text.PrettyPrint.ANSI.Leijen (Doc, dullyellow, hang, hsep, indent, magenta, text, vsep, yellow)

import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Zegloc.Anf

import Zegloc.Name (Global (..), Identifier (..), Local (..), Namespace (..))

prettyAnf :: Anf -> Doc
prettyAnf (Anf bindings result) =
  vsep [ keyword "let"
       , indent 2 (bindings & fmap prettyBinding & Vector.toList & vsep)
       , keyword "in"
       , indent 2 (prettyValue result)
       , keyword "end" ]

prettyBinding :: (Local, Expression) -> Doc
prettyBinding (name, expression) =
  hang 2 $
    hsep [ keyword "val"
         , prettyLocal name
         , keyword "="
         , prettyExpression expression ]

prettyExpression :: Expression -> Doc

prettyExpression (ApplyE e f) =
  hsep [ prettyValue e
       , prettyValue f ]

prettyExpression (LambdaE x e) =
  vsep [ hsep [ keyword "fun"
              , prettyLocal x
              , keyword "->" ]
       , prettyAnf e ]

prettyExpression (EvidencePlaceholderE _) =
  hsep [ keyword "evidence" ]

prettyValue :: Value -> Doc

prettyValue ErrorV =
  keyword "error"

prettyValue (LocalV name) =
  prettyLocal name

prettyValue (GlobalV name) =
  prettyGlobal name

prettyIdentifier :: Identifier -> Doc
prettyIdentifier (Word name) = text (Text.unpack name)
prettyIdentifier (Synthesized name) = text (show name)

prettyNamespace :: Namespace -> Doc
prettyNamespace (Namespace parts) =
  parts & Vector.toList
        & fmap prettyIdentifier
        & intersperse (text "\\")
        & fold

prettyLocal :: Local -> Doc
prettyLocal (Local identifier) =
  dullyellow $ text "$" <> prettyIdentifier identifier

prettyGlobal :: Global -> Doc
prettyGlobal (Global namespace identifier) =
  yellow $ prettyNamespace namespace <> text "\\" <> prettyIdentifier identifier

--------------------------------------------------------------------------------

keyword :: String -> Doc
keyword = magenta . text
