module Data.ShoppingList (
    enable
  , disable
  , disableMulti
  , getEnabled
  , ShoppingList
  , getAllAsJSON
  , getEnabledAsJSON
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Char (toUpper)
import Data.List (foldl')
import Text.JSON

instance JSON T.Text where
  showJSON = showJSON . T.unpack
  readJSON = fmap T.pack . readJSON

type ShoppingList = Map Text Bool

getAsJSON ::  [Text] -> Text
getAsJSON = T.pack . encode

getAllAsJSON :: ShoppingList -> Text
getAllAsJSON = getAsJSON . getAll

getEnabledAsJSON :: ShoppingList -> Text
getEnabledAsJSON = getAsJSON . getEnabled

getAll :: ShoppingList -> [Text]
getAll = M.keys

getEnabled :: ShoppingList -> [Text]
getEnabled = M.keys . M.filter id

-- |The 'titleCase' function turns a text into title case.
-- For example "hello" is changed into "Hello".
titleCase :: Text -> Text
titleCase t =
  let a = toUpper (T.head t)
      xs = T.tail t
  in a `T.cons` xs

enable :: Text -> ShoppingList -> ShoppingList
enable = flip M.insert True . titleCase

disable ::  ShoppingList -> Text -> ShoppingList
disable = flip (M.update (const (Just False)) . titleCase)

disableMulti ::  ShoppingList -> [Text] -> ShoppingList
disableMulti = foldl' disable
