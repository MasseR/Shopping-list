module Handlers.Autocomplete (completeApp) where

import Network.CGI.Text
import Data.ShoppingList
import Types
import Control.Monad.State (lift, get)

autocomplete :: Maybe Text -> ShoppingList -> Text
autocomplete Nothing list = getAllAsPlain list
autocomplete (Just x) list = getFilteredAsPlain x list

completeApp ::  Shop CGIResult
completeApp = do
  setHeader "Content-Type" "text/plain; charset=utf-8"
  input <- getInput "q"
  list <- lift get
  outputText $ autocomplete input list
