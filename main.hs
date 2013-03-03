{-# LANGUAGE OverloadedStrings #-}

import Prelude 
import Text.XML.Cursor
import qualified Text.XML as X
import System.Environment
import qualified Text.HTML.DOM as H (readFile)
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html as B
import qualified Text.Blaze.Html.Renderer.String as R

main = do
  args <- getArgs
  dispatch args

dispatch :: [String] -> IO()
dispatch [] = putStrLn "not enough args";
dispatch xs = do
    str <-  readFile $  head xs
    let contents = T.pack str
        root = fromDocument $ X.parseText_ X.def contents 
        cs = getElem root
    let n = node $ cs!!0
    -- putStrLn $ show n
    putStrLn $ R.renderHtml $ B.toHtml $ removeElem n

getElem :: Cursor -> [Cursor]
getElem root = do
  cs <- descendant root
  element "head" cs

removeElem :: X.Node -> X.Node
removeElem (X.NodeElement (X.Element a b xs)) = X.NodeElement (X.Element a b (filter (\_->False) xs))

addElem :: X.Node -> X.Node
addElem (X.NodeElement (X.Element a b xs)) = X.NodeElement (X.Element a b (xs ++ xs))
