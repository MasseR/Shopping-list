{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where
import Network.CGI.Text
import qualified Data.Text as T
import Data.ShoppingList
import Data.ShoppingList.Persist

autoComplete :: Maybe Text -> ShoppingList -> Text
autoComplete Nothing list = getAllAsJSON list
autoComplete (Just x) list = getFilteredAsJSON x list

cgiMain = do
  list <- liftIO $ loadList
  comp <- getInput "q"
  setHeader "Content-Type" "text/plain; charset=utf-8"
  outputText $ autoComplete comp list

main = runCGI (handleErrors cgiMain)
