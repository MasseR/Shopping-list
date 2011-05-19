{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Handlers.Main (mainApp, itemlist) where

import Types
import qualified Data.Text.Lazy as T
import Network.CGI.Text
import Control.Monad.State
import Data.ShoppingList
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Data.Monoid

check :: [Text] -> Shop CGIResult
check x
  | not (null x) = do
    lift $ modify (disableMulti x)
    redirect "ostoslista.cgi"
  | otherwise = outputNothing

appendNew :: Maybe Text -> Shop CGIResult
appendNew  Nothing = outputNothing
appendNew  (Just x) = do
  lift (modify (enable x))
  redirect "ostoslista.cgi"

mainApp ::  Shop CGIResult
mainApp = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  getInput "append" >>= appendNew
  getMultiInput "list" >>= check
  l <- lift get
  outputFPS $ renderHtml (html l)

html :: ShoppingList -> H.Html
html list = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width;"
    mkstyles styles
    mkscripts scripts
    H.title "Shopping list"
  H.body $ do
    H.div ! A.id "main" $ do
      H.h1 "Shopping list"
      H.div ! A.id "info" $ do
        H.p "0 items hidden" ! A.id "hiddenitems"
        H.a "toggle" ! A.href "#" ! A.id "show"
      H.div ! A.id "form" $ do
        H.form ! A.id "append" ! A.method "POST" $ do
          H.input ! A.id "input" ! A.type_ "text" ! A.name "append"
          H.input ! A.type_ "submit" ! A.value "Add new"
        H.form ! A.id "check" ! A.method "POST" $ do
          H.ul ! A.id "items" $
            itemlist items
          H.input ! A.type_ "submit" ! A.value "Clear selected"
  where
    items = getAssoc list
    styles = ["css/style.css", "css/jquery.autocomplete.css"]
    scripts = reverse [
        "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js"
      , "js/jquery.autocomplete.js"
      , "js/autocomplete.js"
      , "js/ajax.js"]
    mkfun :: (H.AttributeValue -> H.Html) -> [Text] -> H.Html
    mkfun f = foldr ((\ x m -> m `mappend` f x) . H.toValue) mempty
    mkstyles = mkfun css
    mkscripts = mkfun js
    css x = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href x
    js x = H.script mempty ! A.type_ "application/javascript" ! A.src x

itemlist :: [(Text, Int)] -> H.Html
itemlist = foldr (\x m -> m `mappend` item x) mempty . reverse
item ::  Show a => (Text, a) -> H.Html
item x@(a,b) = H.li ((H.input ! A.name "list" ! A.type_ "checkbox" ! A.value (H.toValue a)) `mappend` itemPP x)
itemPP ::  Show a => (Text, a) -> H.Html
itemPP (a,b) = H.toHtml $ T.unwords [T.pack $ show b, "-", a]
