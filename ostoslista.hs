{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where
import Network.CGI.Text
import Text.Hamlet
import Data.ShoppingList
import Data.ShoppingList.Persist
import Transaction

appendNew :: ShoppingList -> Maybe Text -> CGI CGIResult
appendNew _ Nothing = outputNothing
appendNew list (Just x) = do
  liftIO $ saveList $ enable x list
  redirect "ostoslista.cgi"

check :: ShoppingList -> [Text] -> CGI CGIResult
check list x
  | not (null x) = do
    liftIO $ saveList $ disableMulti list x
    redirect "ostoslista.cgi"
  | otherwise = outputNothing

cgiMain :: CGI CGIResult
cgiMain = do
  list <- liftIO $ loadList
  getInput "append" >>= appendNew list
  getMultiInput "list" >>= check list
  setHeader "Content-Type" "text/html; charset=utf-8"
  outputFPS $ renderHtml (html list)

html :: ShoppingList -> Text.Hamlet.Html
html list = let items = getEnabled list in [hamlet|
!!!
<html>
  <head>
    <meta charset=utf-8>
    <style type="text/css">
       #items > li { list-style-type: none; }
    <title>Shopping list
  <body>
    <h1>Shopping list
    <form method=POST>
      <input #addfocus type=text name=append>
      <input type=submit value="Add new">
    <form method=POST>
      <ul id=items>
         $forall item <- items
             <li>
               <input type=checkbox name=list value="#{item}">#{item}
      <input type=submit value="Clear selected">
   <script type="application/javascript">
     window.document.getElementById("addfocus").focus();
  |]

main ::  IO ()
main = withFileTransaction "/tmp/ostoslista.lock" (runCGI (handleErrors cgiMain))
