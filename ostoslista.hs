{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes #-}
module Main where
import Network.CGI.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Data.ShoppingList
import Data.ShoppingList.Persist
import Control.Monad.State
import Control.Monad.CatchIO
import Data.Monoid
import Data.Acid
import qualified Data.Text.Lazy as T

appendNew :: Maybe Text -> CGIT (StateT ShoppingList IO) CGIResult
appendNew Nothing = outputNothing
appendNew (Just x) = do
  lift $ modify (enable x)
  redirect "ostoslista.cgi"

check :: [Text] -> CGIT (StateT ShoppingList IO) CGIResult
check x
  | not (null x) = do
    lift $ modify (disableMulti x)
    redirect "ostoslista.cgi"
  | otherwise = outputNothing

cgiMain :: CGIT (StateT ShoppingList IO) CGIResult
cgiMain = do
  getInput "mode" >>= handler

handler (Just "autocomplete") = completeApp
handler _ = mainApp

autocomplete :: Maybe Text -> ShoppingList -> Text
autocomplete Nothing list = getAllAsPlain list
autocomplete (Just x) list = getFilteredAsPlain x list

completeApp = do
  setHeader "Content-Type" "text/plain; charset=utf-8"
  input <- getInput "q"
  list <- lift get
  outputText $ autocomplete input list

mainApp = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  getInput "append" >>= appendNew
  getMultiInput "list" >>= check
  l <- lift get
  outputFPS $ renderHtml (html l)

html :: ShoppingList -> H.Html
html list = H.docTypeHtml $  do
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
          H.p "0 items hidden" ! A.style "display:none" ! A.id "hiddenitems"
          H.a "show" ! A.href "#" ! A.id "show" ! A.style "display:none"
        H.div ! A.id "form" $ do
          H.form ! A.method "POST" $ do
            H.input ! A.id "input" ! A.type_ "text" ! A.name "append"
            H.input ! A.type_ "submit" ! A.value "Add new"
          H.form ! A.method "POST" $ do
            H.ul ! A.id "items" $
              foldr (\x m -> m `mappend` item x) mempty items
            H.input ! A.type_ "submit" ! A.value "Clear selected"
  where
    items = getAssoc list
    styles = ["css/style.css", "css/jquery.autocomplete.css"]
    scripts = reverse [
        "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js"
      , "js/jquery.autocomplete.js"
      , "js/autocomplete.js"
      , "js/hide.js"]
    mkfun :: (H.AttributeValue -> H.Html) -> [Text] -> H.Html
    mkfun f = foldr (\x m -> m `mappend` f x) mempty . map H.toValue
    mkstyles = mkfun css
    mkscripts = mkfun js
    css x = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href x
    js x = H.script mempty ! A.type_ "application/javascript" ! A.src x
    item x@(a,b) = H.li ((H.input ! A.name "list" ! A.type_ "checkbox" ! A.value (H.toValue a)) `mappend` itemPP x)
    itemPP (a,b) = H.toHtml $ (T.unwords [(T.pack $ show b), "-", a])

--main ::  IO ()
main = do
  a <- openAcidState empty
  loadList a >>= execStateT (runCGI (handleErrors cgiMain )) >>= saveList a
