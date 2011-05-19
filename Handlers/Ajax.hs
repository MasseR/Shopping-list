{-# LANGUAGE OverloadedStrings #-}
module Handlers.Ajax where

import Handlers.Main (itemlist)
import Types
import Network.CGI.Text
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Data.ShoppingList
import Control.Monad.State (modify, get, lift)

append Nothing = return False
append (Just x) = lift (modify (enable x)) >> return True

appendApp :: Shop CGIResult
appendApp = do
  _ <- getInput "append" >>= append
  lift get >>= \l -> outputFPS $ renderHtml (itemlist $ getAssoc l)
