{-# language BlockArguments, ScopedTypeVariables, OverloadedStrings,
  NamedFieldPuns, GeneralizedNewtypeDeriving, DataKinds, DeriveAnyClass,
  RankNTypes, LambdaCase, DerivingStrategies #-}

module Myxine.EventLoop
  ( pageUrl
  , Update(..)
  , sendUpdate
  , withEvents
  , EventParseException(..)
  ) where

import Data.List
import Control.Exception
import Data.Dependent.Map (Some(..))
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Aeson as JSON
import qualified Network.HTTP.Req as Req
import Network.HTTP.Types (ok200)
import Network.HTTP.Client (responseStatus, responseBody)

import Myxine.Event
import Myxine.EventStream
import Myxine.Target

-- | If the response from the server fails to parse, this exception is thrown.
-- This should never happen in ordinary circumstances; if it does, your version
-- of the client library may mismatch the version of the Myxine server you are
-- running.
data EventParseException
  = TargetParseException String
    -- ^ The target path failed to parse
  | UnknownEventTypeException ByteString
    -- ^ The event type failed to parse
  | EventDataParseException ByteString String ByteString
    -- ^ The properties associated with the event type failed to parse
  deriving (Eq, Ord, Show, Exception)

data Update
  = Dynamic (Maybe Text) Text
  | Static ByteString ByteString

pageUrl :: Text -> Req.Url 'Req.Http
pageUrl path =
  foldl' (Req./:) (Req.http "localhost") (Text.split ('/' ==) path)

sendUpdate :: Int -> Text -> Update -> IO ()
sendUpdate port path update =
  do _ <- Req.runReq Req.defaultHttpConfig $
       Req.req Req.POST (pageUrl path) body Req.ignoreResponse options
     pure ()
  where
    body   :: Req.ReqBodyLbs
    params :: Req.Option 'Req.Http
    (body, params) = case update of
      Dynamic maybeTitle text ->
        ( Req.ReqBodyLbs (ByteString.fromStrict (Text.encodeUtf8 text))
        , foldMap ("title" Req.=:) maybeTitle )
      Static contentType content ->
        ( Req.ReqBodyLbs content
        , Req.header "Content-Type" (ByteString.toStrict contentType)
          <> Req.queryFlag "static" )

    options = (Req.port port <> params)

-- | Given a port, path, and list of events, make a request to Myxine for the
-- event stream at that location and run the provided callback for each typed
-- event in the stream. If the list of events specified is empty, every event
-- will be listened for.
--
-- This blocks until the stream is closed by the server, or an exception is
-- encountered. This throws 'EventParseException' if the server sends an
-- unparseable event (this shouldn't happen in normal operation), and
-- 'Req.HttpException' if the underlying connection has an issue.
withEvents ::
  Int ->
  Text ->
  [Some EventType] ->
  (forall d. EventType d -> d -> [Target] -> IO ()) ->
  IO ()
withEvents port path events perEvent =
  do withStreamEvents
       (pageUrl path)
       (Req.port port <> eventParams)
       \StreamEvent{eventId, eventType, eventData} -> do
         targets <- either (throwIO . TargetParseException)
                           pure (JSON.eitherDecode eventId)
         withParsedEvent eventType eventData \case
           Left Nothing -> throwIO (UnknownEventTypeException eventType)
           Left (Just err) -> throwIO (EventDataParseException eventType err eventData)
           Right (eventTy, properties) ->
             perEvent eventTy properties targets
  where
    eventParams :: Req.Option 'Req.Http
    eventParams = case events of
      [] -> Req.queryFlag "events"
      es -> flip foldMap es
        \(Some e) -> "event" Req.=: ByteString.unpack (encodeEventType e)

-- | Given an event name and properties as raw bytestrings, invoke the given
-- callback if the event parses properly, or return 'Nothing'.
withParsedEvent ::
  ByteString -> ByteString -> (forall d. Either (Maybe String) (EventType d, d) -> r) -> r
withParsedEvent name properties k =
  case decodeSomeEventType name of
    Nothing -> k (Left Nothing)
    Just (Some t) -> case decodeEventProperties t properties of
      Left err -> k (Left (Just err))
      Right p -> k (Right (t, p))

-- | Request an event-stream from the given 'Req.Url' with the given
-- 'Req.Option's, and run the provided callback for each 'StreamEvent'. This
-- throws 'Req.HttpException' when the underlying connection has an issue.
withStreamEvents ::
  Req.Url scheme -> Req.Option scheme -> (StreamEvent -> IO ()) -> IO ()
withStreamEvents url options withEvent =
  Req.runReq Req.defaultHttpConfig $ Req.reqBr Req.GET url Req.NoReqBody options
    \response ->
      if responseStatus response /= ok200
      then pure ()
      else do
        let nextChunk = ByteString.fromStrict <$> responseBody response
        nextLine <- linesFromChunks nextChunk
        nextEvent <- eventsFromLines nextLine
        let loop = do
              maybeEvent <- nextEvent
              maybe (pure ()) (\e -> withEvent e >> loop) maybeEvent
        loop
