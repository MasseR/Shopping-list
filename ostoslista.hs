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
    <meta charset=utf-8 />
    <meta name="viewport" content="width=device-width;" />
    <link rel="stylesheet" type="text/css" href="css/style.css" />
    <link rel="stylesheet" type="text/css" href="css/jquery.autocomplete.css" />
    <script type="application/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js">
    <script type="application/javascript" src="js/jquery.autocomplete.js">
    <script type="application/javascript" src="js/autocomplete.js">
    <title>Shopping list
  <body>
    <h1>Shopping list
    <form method=POST>
      <input #input type=text name=append>
      <input type=submit value="Add new">
    <form method=POST>
      <ul id=items>
         $forall item <- items
             <li>
               <input type=checkbox name=list value="#{item}">#{item}
      <input type=submit value="Clear selected">
  |]

main ::  IO ()
main = withFileTransaction "/tmp/ostoslista.lock" (runCGI (handleErrors cgiMain))
