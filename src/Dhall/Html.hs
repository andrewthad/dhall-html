{-# LANGUAGE OverloadedStrings #-}

module Dhall.Html
  ( typeDom
  , Node(..)
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
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Text.XmlHtml as XH
import qualified Control.Exception
import qualified Dhall.Context as DC
import qualified Dhall.Import 
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

domContext :: Context (Expr s X)
domContext = DC.empty
  & DC.insert "Node" (Const Type)
  & DC.insert "nodeText" (Pi "_" Text nodeType)
  & DC.insert "nodeElement"
    (Pi "_" Text (Pi "_" (App List (Record (M.fromList [("name",Text),("value",Text)]))) (Pi "_" (App List nodeType) nodeType)))

-- Node : Type
-- nodeElement : Text -> List {name:Text,value:Text} -> List Node -> Node
-- nodeText : Text -> Node

nodeType :: Expr s X
nodeType = Var (V "Node" 0)

nodeExpr :: Data.Text.Lazy.Text -> IO XH.Node
nodeExpr text = do
  let expected = nodeType
  let delta = Directed "(input)" 0 0 0 0
  expr     <- throws (Dhall.Parser.exprFromText delta text)
  expr'    <- Dhall.Import.load expr
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
  case extractNode (Dhall.Core.normalize expr') of
      Just x  -> return x
      Nothing -> fail "input: malformed `Type`"

throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right r) = return r

extractNode :: Expr X X -> Maybe XH.Node
extractNode x = case x of
  App (Var (V "nodeText" 0)) (TextLit t) -> 
    Just (XH.TextNode (LT.toStrict (TB.toLazyText t)))
  App (App (App (Var (V "nodeElement" 0)) (TextLit t)) (ListLit _ xpairs)) (ListLit _ v) -> do
    pairs <- mapM extractPairs xpairs
    nodes <- mapM extractNode v
    Just (XH.Element (LT.toStrict (TB.toLazyText t)) (V.toList pairs) nodes)
  _ -> Nothing

extractPairs :: Expr X X -> Maybe [(T.Text,T.Text)]
extractPairs = _


-- data Node
--   = NodeText Data.Text.Lazy.Text
--   | NodeElement Data.Text.Lazy.Text (Vector Node)
--   deriving (Show)

