{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Dhall.Html.Parse
  ( parseNodes
  , HtmlError(..)
  ) where

import Dhall.Core
import Data.Maybe
import Control.Monad
import Control.Exception (Exception)
import qualified Text.XmlHtml as XH
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

data Stripped
  = StrippedText !T.Text 
  | StrippedElement !T.Text ![(T.Text,T.Text)] ![Stripped]

data HtmlError
  = HtmlErrorEmpty
  deriving (Show,Eq)

instance Exception HtmlError

stripComments :: XH.Node -> Maybe Stripped
stripComments = \case
  XH.TextNode t -> Just (StrippedText t)
  XH.Comment _ -> Nothing
  XH.Element a b c ->
    Just $ StrippedElement a b (mapMaybe stripComments c)

strippedToExpr :: Stripped -> Either HtmlError (Expr s a)
strippedToExpr = \case
  StrippedText t -> Right (App (Var (V "nodeText" 0)) (TextLit (TB.fromText t)))
  StrippedElement theTag attrs children -> case theTag of
    -- "apply" -> error "handle apply"
    _ -> do
      xs <- mapM strippedToExpr (V.fromList children)
      return $ App (App (App (Var (V "nodeElement" 0)) (TextLit (TB.fromText theTag))) (ListLit (Just attrsType) (attributesToExpr attrs)))
        (ListLit (Just nodeType) xs)


parseNodes :: [XH.Node] -> Either HtmlError (Expr s a)
parseNodes = 
      fmap (ListLit (Just nodeType)) . mapM strippedToExpr
  <=< Right . V.fromList . mapMaybe stripComments

attributeToExpr :: (T.Text,T.Text) -> Expr s a
attributeToExpr (name,value) = RecordLit
  (M.fromList [("name",TextLit $ TB.fromText name),("value",TextLit $ TB.fromText value)])

attributesToExpr :: [(T.Text,T.Text)] -> V.Vector (Expr s a)
attributesToExpr xs = V.map attributeToExpr (V.fromList xs)

nodeType :: Expr s a
nodeType = Var (V "Node" 0)

attrsType :: Expr s a
attrsType = Record (M.fromList [("name",Text),("value",Text)])
