{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Dhall.Html
  ( typeDom
  , nodeExpr
  ) where

import Dhall.Core
import Dhall.TypeCheck
import Dhall.Context (Context)
import Dhall.Parser (Src(..))
import Data.Function
import Data.Monoid
import Data.Text.Buildable (Buildable(..))
import Text.Trifecta.Delta (Delta(..))
import Control.Exception (Exception)
import Data.Vector (Vector)
import Control.Monad
import Control.Applicative (liftA2)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Text.XmlHtml as XH
import qualified Control.Exception
import qualified Dhall.Context as DC
import qualified Dhall.Import 
import qualified Dhall.Html.Import 
import qualified Dhall.Parser
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding

import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

typeDom :: Expr s X -> Either (TypeError s) (Expr s X)
typeDom = typeWith domContext

domContext :: Context (Expr s a)
domContext = DC.empty
  & DC.insert "Node" (Const Type)
  & DC.insert "nodeText" (Pi "_" Text nodeType)
  & DC.insert "nodeElement"
    (Pi "_" Text (Pi "_" (App List attrsType) (Pi "_" (App List nodeType) nodeType)))

-- Node        : Type
-- nodeElement : Text -> List {name:Text,value:Text} -> List Node -> Node
-- nodeText    : Text -> Node

nodeType :: Expr s a
nodeType = Var (V "Node" 0)

attrsType :: Expr s a
attrsType = Record (M.fromList [("name",Text),("value",Text)])

nodeExpr :: Data.Text.Lazy.Text -> IO [XH.Node]
nodeExpr text = do
  let expected = App List nodeType :: Expr Src X
  let delta = Directed "(input)" 0 0 0 0
  expr     <- throws (Dhall.Parser.exprFromText delta text)
  expr'    <- Dhall.Html.Import.loadWith domContext expr
  let suffix =
          ( Data.ByteString.Lazy.toStrict
          . Data.Text.Lazy.Encoding.encodeUtf8
          . Data.Text.Lazy.Builder.toLazyText
          . build
          ) expected
  let annot = case expr' of
          Note (Src begin end bytes) _ ->
              Note (Src begin end bytes') (Annot expr' expected)
            where
              bytes' = bytes <> " : " <> suffix
          _ ->
              Annot expr' expected
  typeExpr <- throws (typeDom annot)
  let normalized = Dhall.Core.normalize expr' :: Expr X X
  case extractNodes normalized of
      Just x  -> return x
      Nothing -> do
        print normalized
        fail "dhall-html internal error, normalization did not work"

throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right r) = return r

extractNodes :: Expr X X -> Maybe [XH.Node]
extractNodes = \case
  ListLit _ exprs -> mapM extractNode (V.toList exprs)
  _ -> Nothing

extractNode :: Expr X X -> Maybe XH.Node
extractNode = \case
  App (Var (V "nodeText" 0)) (TextLit t) -> 
    Just (XH.TextNode (LT.toStrict (TB.toLazyText t)))
  App (App (App (Var (V "nodeElement" 0)) (TextLit t)) (ListLit _ xpairs)) (ListLit _ v) -> do
    pairs <- mapM extractPairs xpairs
    nodes <- mapM extractNode v
    Just (XH.Element (LT.toStrict (TB.toLazyText t)) (V.toList pairs) (V.toList nodes))
  _ -> Nothing

requireTextLiteral :: Expr s a -> Maybe T.Text
requireTextLiteral = \case
  TextLit t -> Just (LT.toStrict (TB.toLazyText t))
  _ -> Nothing

lookupTextLiteral :: 
     LT.Text -- ^ key
  -> M.Map LT.Text (Expr s a) -- ^ map
  -> Maybe T.Text
lookupTextLiteral key = requireTextLiteral <=< M.lookup key

extractPairs :: Expr s a -> Maybe (T.Text,T.Text)
extractPairs = \case
  RecordLit m -> liftA2 (,) (lookupTextLiteral "name" m) (lookupTextLiteral "value" m)
  _ -> Nothing

