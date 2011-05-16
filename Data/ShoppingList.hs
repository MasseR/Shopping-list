{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
-- |Pure interface for the Shopping list
module Data.ShoppingList (
    enable
  , disable
  , disableMulti
  , getEnabled
  , getFiltered
  , ShoppingList
  , getAllAsPlain
  , getFilteredAsPlain
  , getAssoc
  , empty
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Char (toUpper)
import Data.List (foldl')
import Data.Typeable (Typeable)
import Data.SafeCopy (deriveSafeCopy, base)

-- | Type alias for the shopping list.
newtype ShoppingList = S (Map Text Int) deriving (Typeable)
$(deriveSafeCopy 0 'base ''ShoppingList)

-- |Empty shopping list
empty :: ShoppingList
empty = S M.empty

getAsPlain :: [Text] -> Text
getAsPlain = T.unlines

getAllAsPlain :: ShoppingList -> Text
getAllAsPlain = getAsPlain . getAll

getAssoc :: ShoppingList -> [(Text, Int)]
getAssoc (S s) = M.assocs $ M.filter (>0) s
getFilteredAsPlain :: Text -> ShoppingList -> Text
getFilteredAsPlain x = getAsPlain . getFiltered x

-- |Return all the items as text
getAll :: ShoppingList -> [Text]
getAll (S s) = M.keys s

getFiltered :: Text -> ShoppingList -> [Text]
getFiltered x (S s) = M.keys $ M.filterWithKey (\k _ -> T.toLower x `T.isPrefixOf` T.toLower k) s

-- |Return currently enabled items as text
getEnabled :: ShoppingList -> [Text]
getEnabled (S s) = M.keys $ M.filter (> 0) s

-- |The 'titleCase' function turns a text into title case.
-- For example "hello" is changed into "Hello".
titleCase :: Text -> Text
titleCase t =
  let a = toUpper (T.head t)
      xs = T.tail t
  in a `T.cons` xs

-- |Enable an item in the database
enable :: Text -> ShoppingList -> ShoppingList
enable x (S s) = S $ M.alter add (titleCase x) s
  where add Nothing = Just 1
        add (Just x) = Just $ succ x

-- |Disable an item in the database
disable ::  Text -> ShoppingList -> ShoppingList
disable x (S s) = S $ M.update (const (Just 0)) (titleCase x) s

-- |Disable multiple items in the database
disableMulti ::  [Text] -> ShoppingList -> ShoppingList
disableMulti x s = foldr (disable) s x
