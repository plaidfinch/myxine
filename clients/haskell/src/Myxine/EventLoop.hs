{-# language BlockArguments #-}

module Myxine.EventLoop where

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import Data.Text (Text)
import Network.HTTP.Req hiding (responseBody)
import Network.HTTP.Types (ok200)
import Network.HTTP.Client (responseStatus, responseBody)

import Myxine.EventStream

data ClientEvent m a
  = BrowserEvent StreamEvent
  | Action (a -> m a)
  | Shutdown

requestEvents :: Url scheme -> Option scheme -> Req (IO (Maybe StreamEvent))
requestEvents url options =
  reqBr GET url NoReqBody options \response ->
    if responseStatus response /= ok200
    then pure (pure Nothing)
    else do
      let nextChunk = ByteString.fromStrict <$> responseBody response
      nextLine <- linesFromChunks nextChunk
      eventsFromLines nextLine
