-- | A wrapper around the Network.CGI module. Turns the original
-- functions `getInput` and getMultiInput` into their Text equivalents.
module Network.CGI.Text (
    Text
  , outputText
  , getInput
  , getMultiInput
  , module Network.CGI
)
where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as TE(decodeUtf8)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import Network.CGI hiding (getInput, getMultiInput)
import qualified Network.CGI (getInputFPS, getMultiInputFPS)

outputText :: MonadCGI m => Text -> m CGIResult
outputText = output . T.unpack

-- |A wrapper for the `getInput` function. The function decodes the
-- ByteString from `getInputFPS` into Text. The form field must be
-- UTF-8.
getInput ::  MonadCGI m => String -> m (Maybe Text)
getInput s = do
  input <- Network.CGI.getInputFPS s
  return (fmap TE.decodeUtf8 input)

-- |A wrapper for the `getMultiInput` function. The function decodes
-- the ByteString values from `getInputFPS` into a list of Text. The
-- form fields must be UTF-8.
getMultiInput ::  MonadCGI m => String -> m [Text]
getMultiInput s = do
  inputs <- Network.CGI.getMultiInputFPS s
  return (map (TE.decodeUtf8) inputs)
