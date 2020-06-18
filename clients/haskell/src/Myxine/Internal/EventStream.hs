{-| * A generic implementation of @text/event-stream@ parsing

    __Note:__ No end-user of this library needs to use this module; it's exposed
    for testing purposes and is not guaranteed to follow the PVP.
-}
module Myxine.Internal.EventStream
  ( StreamEvent(..)
  , eventsFromLines
  , linesFromChunks
  ) where

import Data.Maybe
import Data.Monoid
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.ByteString.Lazy.Builder as ByteString
import Data.List.NonEmpty (NonEmpty(..))
import Data.IORef

data StreamEvent
  = StreamEvent
    { eventId   :: ByteString
    , eventType :: ByteString
    , eventData :: ByteString
    } deriving (Eq, Ord, Show)

eventsFromLines :: IO (Maybe ByteString) -> IO (IO (Maybe StreamEvent))
eventsFromLines nextLine = do
  current <- newIORef mempty
  pure (go current)
  where
    go :: IORef (Any, Last ByteString, Last ByteString, Endo [ByteString]) -> IO (Maybe StreamEvent)
    go current = nextLine >>= maybe (pure Nothing) \line -> do
      if line == ""
      then tryYield current
      else
        let (field, rest) = ByteString.break (':' ==) line
            value = case ByteString.uncons rest of
              Just (_, rest') -> case ByteString.uncons rest' of
                Just (' ', rest'') -> rest''
                _ -> rest'
              Nothing -> rest
            event' = case field of
              "id"    -> (Any True, pure value, mempty,     mempty)
              "event" -> (Any True, mempty,     pure value, mempty)
              "data"  -> (Any True, mempty,     mempty,     Endo (value :))
              _       -> mempty
        in do
          event <- readIORef current
          writeIORef current (event <> event')
          go current

    tryYield :: IORef (Any, Last ByteString, Last ByteString, Endo [ByteString]) -> IO (Maybe StreamEvent)
    tryYield current = do
      event <- readIORef current
      writeIORef current mempty
      maybe (go current) (pure . Just) (finalize event)

    finalize :: (Any, Last ByteString, Last ByteString, Endo [ByteString]) -> Maybe StreamEvent
    finalize ( Any False, _, _, _ ) = Nothing
    finalize ( Any True
             , Last (fromMaybe "" -> i)
             , Last (fromMaybe "" -> t)
             , Endo (ByteString.intercalate "\n" . ($ []) -> d) ) =
      Just (StreamEvent { eventId = i, eventType = t, eventData = d })

linesFromChunks :: IO ByteString -> IO (IO (Maybe ByteString))
linesFromChunks nextChunk = do
  remainder <- newIORef (Just (Right mempty))
  pure (go remainder)
  where
    go :: IORef (Maybe (Either (NonEmpty ByteString) ByteString.Builder)) -> IO (Maybe ByteString)
    go remainder = readIORef remainder >>= \case
      (Just (Left (unfinished :| []))) -> do
        writeIORef remainder (Just (Right (ByteString.lazyByteString unfinished)))
        go remainder
      (Just (Left (complete :| next : rest))) -> do
        writeIORef remainder (Just (Left (next :| rest)))
        pure (Just complete)
      (Just (Right current)) -> do
        chunk <- nextChunk
        case ByteString.split '\n' chunk of
          [] -> do
            writeIORef remainder Nothing
            go remainder
          [unfinished] -> do
            writeIORef remainder (Just (Right (current <> ByteString.lazyByteString unfinished)))
            go remainder
          (end : next : rest) -> do
            writeIORef remainder (Just (Left (next :| rest)))
            pure (Just (ByteString.toLazyByteString (current <> ByteString.lazyByteString end)))
      Nothing -> pure Nothing