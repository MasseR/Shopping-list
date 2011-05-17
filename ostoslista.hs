{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes #-}
module Main where
import Network.CGI.Text
import Text.Hamlet
import Data.ShoppingList
import Data.ShoppingList.Persist
import Control.Monad.State
import Control.Monad.CatchIO
import Data.Acid

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
  getInput "append" >>= appendNew
  getMultiInput "list" >>= check
  setHeader "Content-Type" "text/html; charset=utf-8"
  l <- lift get
  outputFPS $ renderHtml (html l)

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

--main ::  IO ()
main = do
  a <- openAcidState empty
  loadList a >>= execStateT (runCGI (handleErrors cgiMain )) >>= saveList a
