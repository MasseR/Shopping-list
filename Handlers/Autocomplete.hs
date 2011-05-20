module Handlers.Autocomplete (completeApp) where

import Network.CGI.Text
import Data.ShoppingList
import Types
import Control.Monad.State (lift, get)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H

autocomplete :: Maybe Text -> ShoppingList -> Text
autocomplete Nothing list = getAllAsPlain list
autocomplete (Just x) list = getFilteredAsPlain x list

completeApp ::  Shop CGIResult
completeApp = do
  setHeader "Content-Type" "text/plain; charset=utf-8"
  input <- getInput "q"
  list <- lift get
  outputFPS $ renderHtml $ H.toHtml $ autocomplete input list
