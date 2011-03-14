{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Network.CGI
import Text.Hamlet
import qualified Data.ByteString.Char8 as BS8
import System.Directory (copyFile)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (foldl')
import Data.Char (toUpper)
import Control.Exception.Base (bracket)
import System.IO (openTempFile, hClose)
import Transaction

type ShoppingList = Map String Bool

titleCase (a:xs) = toUpper a : xs

appendNew :: ShoppingList -> Maybe String -> CGI CGIResult
appendNew _ Nothing = outputNothing
appendNew list (Just x) = liftIO (safeWrite $ BS8.pack $ show (M.insert (titleCase x) True list)) >> redirect "ostoslista.cgi"

safeWrite :: BS8.ByteString -> IO ()
safeWrite c = bracket (openTempFile "/tmp" "list.tmp")
    (\(path, h) -> hClose h >> copyFile path "/tmp/list")
    (\(_, h) -> BS8.hPutStr h c)

check :: ShoppingList -> [String] -> CGI CGIResult
check list x | not (null x) = do
  liftIO $ safeWrite $ BS8.pack $
	show $ foldl' (flip (M.update (const (Just False)) . titleCase)) list x
  redirect "ostoslista.cgi"
	     | otherwise = outputNothing

cgiMain :: CGI CGIResult
cgiMain = do
  list <- (read . BS8.unpack) `fmap` (liftIO $ BS8.readFile "/tmp/list")
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
