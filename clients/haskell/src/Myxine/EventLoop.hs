{-# language BlockArguments, ScopedTypeVariables, OverloadedStrings,
  NamedFieldPuns, GeneralizedNewtypeDeriving, DataKinds, DeriveAnyClass,
  RankNTypes, LambdaCase, DerivingStrategies #-}

module Myxine.EventLoop
  ( Update(..)
  , sendUpdate
  , withEvents
  , EventParseException(..)
  ) where

import Control.Monad
import Control.Concurrent
import Data.List
import Control.Exception
import Data.Dependent.Map (Some(..))
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
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
-- running, or there may be a bug in the Myxine server or this library.
--
-- If you encounter this exception in the wild, please file a bug report at
-- <https://github.com/GaloisInc/myxine/issues/new>. Thanks!
data EventParseException
  = TargetParseException String
    -- ^ The target path failed to parse
  | UnknownEventTypeException ByteString
    -- ^ The event type failed to parse
  | EventDataParseException ByteString String ByteString
    -- ^ The properties associated with the event type failed to parse
  deriving (Eq, Ord, Exception)

instance Show EventParseException where
  show exn =
       "*** Myxine client panic: Failed to parse " <> component <> "! This means one of:\n\n"
    <> "  1) You connected to an event source that is not the Myxine server\n"
    <> "  2) You connected to a Myxine server process with an incompatible major version\n"
    <> "  3) There is a bug in the Myxine server or client library (totally possible!)\n\n"
    <> "  If you suspect it's (3), please file a bug report at:\n\n    " <> bugReportURL <> "\n\n"
    <> "  Please include the version of this library, the version of the Myxine server,\n"
    <> "  and the following details:\n\n"
    <> details
    where
      component, details, bugReportURL :: String
      (component, details) = case exn of
        TargetParseException input ->
          ("target path",
           "  - Unparseable target path: " <> show input)
        UnknownEventTypeException eventType ->
          ("event type",
           "  - Unknown event type: " <> show eventType)
        EventDataParseException eventType parseError badInput ->
          ("event properties",
           "  - Known event type: " <> show eventType <> "\n" <>
           "  - Parse error: " <> parseError <> "\n" <>
           "  - Bad input properties: " <> show badInput)
      bugReportURL = "https://github.com/GaloisInc/myxine/issues/new"

-- | A full page update as ready-to-send to the Myxine server.
data Update
  = Dynamic       -- ^ A dynamic page which can be updated live
    (Maybe Text)  -- ^ An optional @<title>@ for the page
    ByteString    -- ^ The contents of the page's @<body>@
  | Static        -- ^ A static file which is hosted precisely as specified
    ByteString    -- ^ The @Content-Type@ of the content
    ByteString    -- ^ The raw bytes to be served by the server

-- | Compute the localhost 'Req.Url' of a given path, allowing for dynamic
-- appearances of @/@ in the URL.
pageUrl :: Text -> Req.Url 'Req.Http
pageUrl path =
  foldl' (Req./:) (Req.http "localhost") (Text.split ('/' ==) path)

-- | Send a full-page update to Myxine at a particular port and path. An
-- 'Update' is either a 'Dynamic' page body with an optional title, or a
-- 'Static' file with a particular Content-Type.
sendUpdate :: Int -> Text -> Update -> IO ()
sendUpdate port path update =
  do _ <- Req.runReq Req.defaultHttpConfig $
       Req.req Req.POST (pageUrl path) body
         Req.ignoreResponse (Req.port port <> params)
     pure ()
  where
    body   :: Req.ReqBodyLbs
    params :: Req.Option 'Req.Http
    (body, params) = case update of
      Dynamic maybeTitle text ->
        ( Req.ReqBodyLbs text
        , foldMap ("title" Req.=:) maybeTitle )
      Static contentType content ->
        ( Req.ReqBodyLbs content
        , Req.header "Content-Type" (ByteString.toStrict contentType)
          <> Req.queryFlag "static" )

-- | Given a port, path, and list of events, make a request to Myxine for the
-- event stream at that location and run the provided callback for each typed
-- event in the stream. If no list of events is specified (@Nothing@), every
-- event will be listened for. If the specific list of no events (@Just []@) is
-- specified, this function will sleep forever.
--
-- This blocks until the stream is closed by the server, or an exception is
-- encountered. This throws 'EventParseException' if the server sends an
-- unparseable event (this shouldn't happen in normal operation), and
-- 'Req.HttpException' if the underlying connection has an issue.
withEvents ::
  Int ->
  Text ->
  Maybe [Some EventType] ->
  (forall d. EventType d -> d -> [Target] -> IO ()) ->
  IO ()
withEvents _ _ (Just []) _ =
  -- if the user requests no events, there is no corresponding Myxine API
  -- request to handle this, so we just sleep forever
  forever (threadDelay maxBound)
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
      Nothing -> Req.queryFlag "events"
      Just es -> flip foldMap es
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
