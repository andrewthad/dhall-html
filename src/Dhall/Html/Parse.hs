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
import GHC.OldList as L
import qualified Text.XmlHtml as XH
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

data Stripped
  = StrippedText !T.Text 
  | StrippedElement !T.Text ![(T.Text,T.Text)] ![Stripped]

data HtmlError
  = HtmlErrorEmpty
  | HtmlErrorLetTextChild
  | HtmlErrorLetNodeChild !T.Text
  | HtmlErrorLetAttrs
  | HtmlErrorLetMissingIn
  | HtmlErrorLetChildrenAfterIn
  | HtmlErrorLetBindAttributes
  | HtmlErrorVarMissingName
  deriving (Show,Eq)

instance Exception HtmlError

stripComments :: XH.Node -> Maybe Stripped
stripComments = \case
  XH.TextNode t -> Just (StrippedText t)
  XH.Comment _ -> Nothing
  XH.Element a b c ->
    Just $ StrippedElement a b (mapMaybe stripComments c)

strippedToExprList :: [Stripped] -> Either HtmlError (Expr s a)
strippedToExprList children =
  fmap (L.foldr ListAppend (ListLit (Just nodeType) V.empty)) (mapM strippedToExpr children)

strippedToExpr :: Stripped -> Either HtmlError (Expr s a)
strippedToExpr = \case
  StrippedText t -> Right (ListLit (Just nodeType) (V.singleton (App (Var (V "nodeText" 0)) (TextLit (TB.fromText t)))))
  StrippedElement theTag attrs children -> case theTag of
    "let" -> letGroup children
    "var" -> case L.lookup "name" attrs of
      Just name -> Right $ Var $ V (LT.fromStrict name) 0
      Nothing -> Left HtmlErrorVarMissingName
    _ -> do
      xs <- mapM strippedToExpr children
      return $ ListLit Nothing $ V.singleton
        ( App 
          (App (App (Var (V "nodeElement" 0)) (TextLit (TB.fromText theTag))) (ListLit (Just attrsType) (attributesToExpr attrs)))
          (L.foldr ListAppend (ListLit (Just nodeType) V.empty) xs)
        )
letGroup :: [Stripped] -> Either HtmlError (Expr s a)
letGroup ys = case ys of
  [] -> Left HtmlErrorLetMissingIn
  x : xs -> case x of
    StrippedText t -> if T.null (T.strip t)
      then letGroup xs
      else Left HtmlErrorLetTextChild
    StrippedElement name attrs children -> case name of
      "bind" -> case L.lookup "name" attrs of
        Just bindingName -> do
          val <- strippedToExprList children
          otherLets <- letGroup xs
          return (Let (LT.fromStrict bindingName) Nothing val otherLets)
        Nothing -> Left HtmlErrorLetBindAttributes
      "in" -> if allWhitespace xs
        then strippedToExprList children
        else Left HtmlErrorLetChildrenAfterIn
      _ -> Left (HtmlErrorLetNodeChild name)

allWhitespace :: [Stripped] -> Bool
allWhitespace ys = case ys of
  [] -> True
  x : xs -> case x of
    StrippedText t -> if T.null (T.strip t)
      then allWhitespace xs
      else False
    StrippedElement _ _ _ -> False

parseNodes :: [XH.Node] -> Either HtmlError (Expr s a)
parseNodes = strippedToExprList <=< Right . mapMaybe stripComments

attributeToExpr :: (T.Text,T.Text) -> Expr s a
attributeToExpr (name,value) = RecordLit
  (M.fromList [("name",TextLit $ TB.fromText name),("value",TextLit $ TB.fromText value)])

attributesToExpr :: [(T.Text,T.Text)] -> V.Vector (Expr s a)
attributesToExpr xs = V.map attributeToExpr (V.fromList xs)

nodeType :: Expr s a
nodeType = Var (V "Node" 0)

attrsType :: Expr s a
attrsType = Record (M.fromList [("name",Text),("value",Text)])
