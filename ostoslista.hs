{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Network.CGI
import Text.Hamlet
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BS8
import System.Directory (doesFileExist)

type ShoppingList = [(Int, String)]

mkIfNotExist = do
  f <- doesFileExist "list"
  if not f
     then BS8.writeFile "list" $ BS8.pack $ show ([] :: ShoppingList)
     else return ()

appendNew :: ShoppingList -> Maybe String -> CGI CGIResult
appendNew _ Nothing = outputNothing
appendNew list (Just x) = liftIO (BS8.writeFile "list" $ BS8.pack $ show ((1, x) : list)) >> redirect "ostoslista.cgi"

check :: ShoppingList -> [String] -> CGI CGIResult
check list x | not (null x) = do
  liftIO $ BS8.writeFile "list" $ BS8.pack $ show $ filter (\(_, y) -> not $ y `elem` x) list
  redirect "ostoslista.cgi"
	     | otherwise = outputNothing

cgiMain :: CGI CGIResult
cgiMain = do
  liftIO mkIfNotExist
  list <- (read . BS8.unpack) `fmap` (liftIO $ BS8.readFile "list")
  getInput "append" >>= appendNew list
  getMultiInput "list" >>= check list
  outputFPS $ renderHtml (html list)

html :: ShoppingList -> Text.Hamlet.Html
html list = [hamlet|
!!!
<html>
  <head>
    <title>Shopping list
  <body>
    <h1>Shopping list
    <form method=POST>
      <input type=text name=append>
      <input type=submit>
    <form method=POST>
      <ul>
	  $forall item <- list
	    <li>
	      <input type=checkbox name=list value="#{snd item}">#{fst item} #{snd item}
      <input type=submit>
  |]

main ::  IO ()
main = runCGI (handleErrors cgiMain)
