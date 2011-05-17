{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where
import Network.CGI.Text
import qualified Data.Text as T
import Data.ShoppingList
import Data.ShoppingList.Persist
import Data.Acid (openAcidState)

autoComplete :: Maybe Text -> ShoppingList -> Text
autoComplete Nothing list = getAllAsPlain list
autoComplete (Just x) list = getFilteredAsPlain x list

cgiMain = do
  list <- liftIO $ openAcidState empty >>= loadList
  comp <- getInput "q"
  setHeader "Content-Type" "text/plain; charset=utf-8"
  outputText $ autoComplete comp list

main = runCGI (handleErrors cgiMain)
