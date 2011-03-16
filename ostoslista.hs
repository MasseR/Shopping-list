{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Network.CGI
import Text.Hamlet
import qualified Data.ByteString.Char8 as BS8
import System.Directory
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (foldl')
import Data.Char (toUpper)
import Control.Exception.Base (bracket)
import System.IO (openTempFile, hClose)
import System.IO.Error (isDoesNotExistError)
import qualified Data.Text.Lazy.Encoding as TE(decodeUtf8)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import Transaction

type ShoppingList = Map T.Text Bool

titleCase t =
  let a = toUpper (T.head t)
      xs = T.tail t
  in a `T.cons` xs

appendNew :: ShoppingList -> Maybe T.Text -> CGI CGIResult
appendNew _ Nothing = outputNothing
appendNew list (Just x) = liftIO (safeWrite $ T.pack $ show (M.insert (titleCase x) True list)) >> redirect "ostoslista.cgi"

safeWrite :: T.Text -> IO ()
safeWrite c = bracket (openTempFile "/tmp" "list.tmp")
    (\(path, h) -> hClose h
      >> copyFile path "/tmp/list"
      >> removeFile path)
    (\(_, h) -> TI.hPutStr h c)

safeRead :: FilePath -> IO BS8.ByteString
safeRead path = catch (BS8.readFile path) (\e -> if isDoesNotExistError e then return "fromList []" else ioError e)

check :: ShoppingList -> [T.Text] -> CGI CGIResult
check list x | not (null x) = do
  liftIO $ safeWrite $ T.pack $
	show $ foldl' (flip (M.update (const (Just False)) . titleCase)) list x
  redirect "ostoslista.cgi"
	     | otherwise = outputNothing

cgiMain :: CGI CGIResult
cgiMain = do
  list <- (read . BS8.unpack) `fmap` (liftIO $ safeRead "/tmp/list")
  getInputFPS "append" >>= \x -> appendNew list (fmap TE.decodeUtf8 x)
  getMultiInputFPS "list" >>= check list . map TE.decodeUtf8
  setHeader "Content-Type" "text/html; charset=utf-8"
  outputFPS $ renderHtml (html list)

html :: ShoppingList -> Text.Hamlet.Html
html list = let items = M.keys (M.filter id list) in [hamlet|
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
