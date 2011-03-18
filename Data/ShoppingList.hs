-- |Pure interface for the Shopping list
module Data.ShoppingList (
    enable
  , disable
  , disableMulti
  , getEnabled
  , getFiltered
  , ShoppingList
  , getAllAsJSON
  , getEnabledAsJSON
  , getFilteredAsJSON
  , empty
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

-- | Type alias for the shopping list.
type ShoppingList = Map Text Bool

-- |Empty shopping list
empty :: ShoppingList
empty = M.empty

-- |Return a list of items as JSON
getAsJSON ::  [Text] -> Text
getAsJSON = T.pack . encode

-- |Return all the items from history as JSON
getAllAsJSON :: ShoppingList -> Text
getAllAsJSON = getAsJSON . getAll

-- |Return currently enabled items as JSON
getEnabledAsJSON :: ShoppingList -> Text
getEnabledAsJSON = getAsJSON . getEnabled

getFilteredAsJSON ::  Text -> ShoppingList -> Text
getFilteredAsJSON = (getAsJSON .) . getFiltered

-- |Return all the items as text
getAll :: ShoppingList -> [Text]
getAll = M.keys

getFiltered :: Text -> ShoppingList -> [Text]
getFiltered x s = M.keys $ M.filterWithKey (\k _ -> T.toLower x `T.isPrefixOf` T.toLower k) s

-- |Return currently enabled items as text
getEnabled :: ShoppingList -> [Text]
getEnabled = M.keys . M.filter id

-- |The 'titleCase' function turns a text into title case.
-- For example "hello" is changed into "Hello".
titleCase :: Text -> Text
titleCase t =
  let a = toUpper (T.head t)
      xs = T.tail t
  in a `T.cons` xs

-- |Enable an item in the database
enable :: Text -> ShoppingList -> ShoppingList
enable = flip M.insert True . titleCase

-- |Disable an item in the database
disable ::  ShoppingList -> Text -> ShoppingList
disable = flip (M.update (const (Just False)) . titleCase)

-- |Disable multiple items in the database
disableMulti ::  ShoppingList -> [Text] -> ShoppingList
disableMulti = foldl' disable
