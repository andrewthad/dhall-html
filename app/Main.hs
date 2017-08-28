module Main where

import qualified Dhall
import qualified Data.Text.Lazy.IO
import qualified Text.XmlHtml as XH
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Dhall.Html (nodeExpr)

main :: IO ()
main = do
  code <- Data.Text.Lazy.IO.getContents
  nodes <- nodeExpr code
  LBC.putStrLn $ BB.toLazyByteString $ XH.renderHtmlFragment XH.UTF8 nodes


