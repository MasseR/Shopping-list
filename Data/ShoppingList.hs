module Data.ShoppingList (
    enable
  , disable
  , disableMulti
  , getEnabled
  , ShoppingList
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Char (toUpper)
import Data.List (foldl')

type ShoppingList = Map Text Bool

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
