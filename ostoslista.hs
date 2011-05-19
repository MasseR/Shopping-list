{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where
import Network.CGI.Text
import Data.ShoppingList
import Data.ShoppingList.Persist
import Control.Monad.State
import Control.Monad.CatchIO
import Data.Acid
import qualified Data.Text.Lazy as T
import Handlers.Main
import Handlers.Autocomplete
import Handlers.Ajax
import Types

cgiMain :: Shop CGIResult
cgiMain = getInput "mode" >>= handler

handler :: Maybe Text -> Shop CGIResult
handler (Just "autocomplete") = completeApp
handler (Just "ajax") = ajaxApp
handler _ = mainApp


--main ::  IO ()
main = do
  a <- openAcidState empty
  loadList a >>= execStateT (runCGI (handleErrors cgiMain )) >>= saveList a
