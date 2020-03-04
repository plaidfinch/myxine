{-| * A generic implementation of @text/event-stream@ parsing

    __Note:__ No end-user of this library needs to use this module; it's exposed
    for testing purposes and is not guaranteed to follow the PVP.
-}
module Myxine.Internal.EventStream
  ( linesFromChunks
  ) where

import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Builder as ByteString
import Data.List.NonEmpty (NonEmpty(..))
import Data.IORef

linesFromChunks :: IO ByteString -> IO (IO (Maybe ByteString.Lazy.ByteString))
linesFromChunks nextChunk = do
  remainder <- newIORef (Just (Right mempty))
  pure (go remainder)
  where
    go :: IORef (Maybe (Either (NonEmpty ByteString) ByteString.Builder)) -> IO (Maybe ByteString.Lazy.ByteString)
    go remainder = readIORef remainder >>= \case
      (Just (Left (unfinished :| []))) -> do
        writeIORef remainder (Just (Right (ByteString.byteString unfinished)))
        go remainder
      (Just (Left (complete :| next : rest))) -> do
        writeIORef remainder (Just (Left (next :| rest)))
        pure (Just (ByteString.Lazy.fromStrict complete))
      (Just (Right current)) -> do
        chunk <- nextChunk
        case ByteString.split '\n' chunk of
          [] -> do
            writeIORef remainder Nothing
            go remainder
          [unfinished] -> do
            writeIORef remainder (Just (Right (current <> ByteString.byteString unfinished)))
            go remainder
          (end : next : rest) -> do
            writeIORef remainder (Just (Left (next :| rest)))
            pure (Just (ByteString.toLazyByteString (current <> ByteString.byteString end)))
      Nothing -> pure Nothing
