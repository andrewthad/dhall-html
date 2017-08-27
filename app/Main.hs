module Main where

import qualified Dhall
import qualified Data.Text.Lazy.IO
import Dhall.Html (nodeExpr)

main :: IO ()
main = do
  code <- Data.Text.Lazy.IO.getContents
  node <- nodeExpr code
  print node


