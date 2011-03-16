{-# LANGUAGE OverloadedStrings #-}
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

safeWrite :: Text -> IO ()
safeWrite c = bracket (openTempFile "/tmp" "list.tmp")
    (\(path, h) -> hClose h
      >> copyFile path "/tmp/list"
      >> removeFile path)
    (\(_, h) -> TI.hPutStr h c)

safeRead :: FilePath -> IO Text
safeRead path = catch (TI.readFile path) (\e -> if isDoesNotExistError e then return "fromList []" else ioError e)

serializeList ::  ShoppingList -> Text
serializeList = T.pack . show

saveList :: ShoppingList -> IO ()
saveList = safeWrite . serializeList

loadList ::  IO ShoppingList
loadList = (read . T.unpack) `fmap` (safeRead "/tmp/list")
