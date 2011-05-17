{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
-- |Utilities for saving and loading shoppinglists
module Data.ShoppingList.Persist (
    saveList
  , loadList
) where

import Data.ShoppingList
import Data.Text.Lazy (Text)
import Control.Exception.Base (bracket)
import System.IO (openTempFile, hClose)
import System.IO.Error (isDoesNotExistError)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import System.Directory
import Data.Acid
import Control.Monad.State (put)
import Control.Monad.Reader (ask)


dataFile = "list.db"

save :: ShoppingList -> Update ShoppingList ()
save = put

load :: Query ShoppingList ShoppingList
load = ask

$(makeAcidic ''ShoppingList ['save, 'load])

saveList :: AcidState (EventState Save)-> ShoppingList-> IO (EventResult Save)
saveList c s = update c (Save s)
loadList ::  AcidState (EventState Load) -> IO (EventResult Load)
loadList c = query c Load
