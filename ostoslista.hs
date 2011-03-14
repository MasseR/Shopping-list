{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Network.CGI
import Text.Hamlet
import Text.Cassius
import qualified Data.ByteString.Char8 as BS8
import System.Directory (doesFileExist)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (foldl')
import System.Environment

type ShoppingList = Map String Bool

appendNew :: ShoppingList -> Maybe String -> CGI CGIResult
appendNew _ Nothing = outputNothing
appendNew list (Just x) = liftIO (BS8.writeFile "list" $ BS8.pack $ show (M.insert x True list)) >> redirect "ostoslista.cgi"

check :: ShoppingList -> [String] -> CGI CGIResult
check list x | not (null x) = do
  liftIO $ BS8.writeFile "list" $ BS8.pack $ show $ foldl' (\a b -> M.update (\_ -> Just False) b a) list x
  redirect "ostoslista.cgi"
	     | otherwise = outputNothing

cgiMain :: CGI CGIResult
cgiMain = do
  list <- (read . BS8.unpack) `fmap` (liftIO $ BS8.readFile "list")
  getInput "append" >>= appendNew list
  getMultiInput "list" >>= check list
  outputFPS $ renderHtml (html list)

html :: ShoppingList -> Text.Hamlet.Html
html list = let items = M.keys (M.filter id list) in [hamlet|
!!!
<html>
  <head>
    <style type="text/css">
       #items > li { list-style-type: none; }
    <title>Shopping list
  <body>
    <h1>Shopping list
    <form method=POST>
      <input type=text name=append>
      <input type=submit value="Add new">
    <form method=POST>
      <ul id=items>
         $forall item <- items
             <li>
               <input type=checkbox name=list value="#{item}">#{item}
      <input type=submit value="Clear selected">
  |]

main ::  IO ()
main = runCGI (handleErrors cgiMain)
