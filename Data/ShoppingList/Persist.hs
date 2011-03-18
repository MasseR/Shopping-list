{-# LANGUAGE OverloadedStrings #-}
-- |Utilities for saving and loading shoppinglists
module Data.ShoppingList.Persist (
    serializeList
  , saveList
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

-- |Safely write to disk using temporary files.
-- The files are copied only if there were no errors
safeWrite :: Text -> IO ()
safeWrite c = bracket (openTempFile "/tmp" "list.tmp")
    (\(path, h) -> hClose h
      >> copyFile path "/tmp/list"
      >> removeFile path)
    (\(_, h) -> TI.hPutStr h c)

-- |Safely read from disk. If the file doesn't exist, empty is returned
safeRead :: FilePath -> IO Text
safeRead path = catch (TI.readFile path) (\e -> if isDoesNotExistError e then return (T.pack $ show empty) else ioError e)

-- |Serialize the list as text
serializeList ::  ShoppingList -> Text
serializeList = T.pack . show

-- |Save the list. Implementation is not defined. Currently works by saving to
-- a file
saveList :: ShoppingList -> IO ()
saveList = safeWrite . serializeList

-- |Loads the list. Implementation is not defined. Currently works by loading
-- from a file
loadList ::  IO ShoppingList
loadList = (read . T.unpack) `fmap` (safeRead "/tmp/list")
