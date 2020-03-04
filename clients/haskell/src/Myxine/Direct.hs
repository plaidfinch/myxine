{-|
Description : Direct one-to-one typed bindings to the Myxine server API

Usually, it makes the most sense to run a Myxine application using the
'Myxine.Page.Page' abstraction in the main module. However, this reactive
model-view-controller approach may not be appropriate for all needs. The
functions below are a one-to-one mapping to the API of the Myxine server.

Like the Myxine server API itself, this interface has a small surface area. You
can send a new page 'Update' using 'sendUpdate', you can loop over all page
events using 'withEvents', and you can evaluate raw JavaScript using
'evaluateJs'.
-}
module Myxine.Direct
  ( -- * Page locations on @localhost@
    PageLocation, pagePort, PagePort, pagePath, PagePath
    -- * Sending updates to pages
  , Update(..), EventList(..), PageEvent(..)
  , PageContent, pageBody, pageTitle, sendUpdate
    -- * Evaluating raw JavaScript in the context of a page
  , JavaScript(..), evaluateJs
    -- * Exceptions
  , EventParseException(..)
    -- * The @Some@ existential
  , Some(..)
  ) where

import Data.Maybe
import Data.Monoid
import Data.String
import Data.List
import Data.Constraint
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
import Myxine.Internal.EventStream
import Myxine.Target

-- | If the response from the server fails to parse, this exception is thrown.
-- This should never happen in ordinary circumstances; if it does, your version
-- of the client library may mismatch the version of the Myxine server you are
-- running, or there may be a bug in the Myxine server or this library.
--
-- If you encounter this exception in the wild, please file a bug report at
-- <https://github.com/GaloisInc/myxine/issues/new>. Thanks!
newtype EventParseException
  = EventParseException String
  deriving (Eq, Ord, Exception)

instance Show EventParseException where
  show (EventParseException details) =
       "*** Myxine client panic: Failed to parse event! This means one of:\n\n"
    <> "  1) You connected to an event source that is not the Myxine server\n"
    <> "  2) You connected to a Myxine server process with an incompatible version\n"
    <> "  3) There is a bug in the Myxine server or client library (totally possible!)\n\n"
    <> "  If you suspect it's (3), please file a bug report at:\n\n    " <> bugReportURL <> "\n\n"
    <> "  Please include the version of this library, the version of the Myxine server,\n"
    <> "  and the following error message:\n\n"
    <> details
    where
      bugReportURL :: String
      bugReportURL = "https://github.com/GaloisInc/myxine/issues/new"

-- | The view of a page, as rendered in the browser. Create page content with
-- 'pageBody' and 'pageTitle', and combine content using the 'Semigroup'
-- instance.
--
-- __Note:__ The 'Semigroup' instance for 'PageContent' takes the last specified
-- 'pageTitle' (if any), and concatenates in order each specified 'pageBody'.
data PageContent
  = PageContent
    { pageContentBody  :: Text
    , pageContentTitle :: Last Text
    } deriving (Eq, Ord, Show)

instance Semigroup PageContent where
  PageContent body title <> PageContent body' title' =
    PageContent (body <> body') (title <> title')

instance Monoid PageContent where
  mempty = PageContent mempty mempty

-- | Create a rendered 'PageContent' with an empty @title@ and the specified
-- text as its @body@.
pageBody :: Text -> PageContent
pageBody body = mempty { pageContentBody = body }

-- | Create a rendered 'PageContent' with an empty @body@ and the specified
-- text as its @title@.
pageTitle :: Text -> PageContent
pageTitle title = mempty { pageContentTitle = Last (Just title) }

-- | A full page update as ready-to-send to the Myxine server.
data Update
  = Dynamic       -- ^ A dynamic page which can be updated live
    PageContent
    -- ^ The content of the page (create this using 'pageBody' and/or
    -- 'pageTitle', combining using the 'Semigroup' instance)
  | Static        -- ^ A static file which is hosted precisely as specified
    ByteString    -- ^ The @Content-Type@ of the content
    ByteString    -- ^ The raw bytes to be served by the server
  deriving (Eq, Ord, Show)

-- | Compute the localhost 'Req.Url' of a given path, allowing for dynamic
-- appearances of @/@ in the URL.
pageUrl :: PagePath -> Req.Url 'Req.Http
pageUrl (PagePath p) =
  foldl' (Req./:) (Req.http "localhost") (Text.split ('/' ==) p)

-- | A local port at which the server is expected to be running. Create one
-- using an integer literal or 'fromInteger'.
newtype PagePort
  = PagePort Int
  deriving newtype (Num, Eq, Ord, Show)

-- | A path at "localhost/..." at which to perform some action. Create one using
-- a string literal or 'Data.String.fromString'.
newtype PagePath
  = PagePath Text
  deriving newtype (IsString, Eq, Ord, Show)

-- | The options for connecting to the Myxine server. This is an opaque
-- 'Monoid': set options by combining 'pagePort' and/or 'pagePath' using their
-- 'Semigroup' instance.
data PageLocation
  = PageLocation
  { pageLocationPort :: Last PagePort
  , pageLocationPath :: Last PagePath
  } deriving (Eq, Ord, Show)

-- | Set the path to something other than the default of @/@.
pagePath :: PagePath -> PageLocation
pagePath p = mempty { pageLocationPath = Last (Just p) }

-- | Set the port to a non-default port. This is only necessary when Myxine is
-- running on a non-default port also.
pagePort :: PagePort -> PageLocation
pagePort p = mempty { pageLocationPort = Last (Just p) }

-- | The default port for the Myxine server
defaultPort :: Int
defaultPort = 1123

instance Semigroup PageLocation where
  PageLocation port1 path1 <> PageLocation port2 path2 =
    PageLocation (port1 <> port2) (path1 <> path2)

instance Monoid PageLocation where
  mempty = PageLocation
    { pageLocationPort = mempty
    , pageLocationPath = mempty }

data PageEvent where
  PageEvent :: { event :: EventType props
               , properties :: props
               , targets :: [Target]
               } -> PageEvent

instance JSON.FromJSON PageEvent where
  parseJSON = JSON.withObject "PageEvent" \o ->
    do eventName <- o JSON..: "event"
       Some event <-
         flip maybe pure
          (fail ("Unrecognized event: " <> Text.unpack eventName))
          (decodeSomeEventType eventName)
       Dict <- pure (eventPropertiesDecodeDict event)
       properties <- o JSON..: "properties"
       targets <- o JSON..: "targets"
       pure (PageEvent{event, properties, targets})

-- | A list of event types to listen for: either all events, or a specific list
-- of events.
data EventList
  = AllEvents  -- ^ Listen for all events
  | SomeEvents [Some EventType]  -- ^ Listen only for these events
  deriving (Eq, Ord, Show)

-- | Send a full-page update to Myxine at a particular port and path. An
-- 'Update' is either a 'Dynamic' page body with an optional title, or a
-- 'Static' file with a particular Content-Type.
sendUpdate ::
  PageLocation {- ^ The location of the page to update -} ->
  Update {- ^ The new content of the page to display -} ->
  EventList {- ^ The events to listen for within the context of this update -} ->
  IO (IO (Maybe PageEvent))
sendUpdate PageLocation{pageLocationPort = Last maybePort,
                        pageLocationPath = Last maybePath} update events =
  do Req.runReq Req.defaultHttpConfig $
       Req.reqBr Req.POST url body (portOption <> updateOptions <> eventOptions)
         \response ->
           (maybe (pure Nothing)
             (either (throwIO . EventParseException) pure . JSON.eitherDecode) =<<)
           <$> linesFromChunks (responseBody response)
  where
    url = pageUrl (fromMaybe "" maybePath)
    portOption = Req.port (maybe defaultPort (\(PagePort p) -> p) maybePort)
    eventOptions = eventParams events

    body :: Req.ReqBodyLbs
    updateOptions :: Req.Option 'Req.Http
    (body, updateOptions) = case update of
      Dynamic (PageContent{pageContentTitle = Last maybeTitle,
                           pageContentBody = text}) ->
        ( Req.ReqBodyLbs (ByteString.fromStrict (Text.encodeUtf8 text))
        , foldMap ("title" Req.=:) maybeTitle )
      Static contentType content ->
        ( Req.ReqBodyLbs content
        , Req.header "Content-Type" (ByteString.toStrict contentType)
          <> Req.queryFlag "static" )

-- | A piece of raw JavaScript to evaluate: either an expression or a block of
-- statements. Expressions need not terminate with a @return@ statement but
-- cannot span multiple lines; block need to have an explicit @return@, but can
-- contain multiple statements and lines.
data JavaScript
  = JsExpression Text -- ^ A JavaScript expression
  | JsBlock Text      -- ^ A block of JavaScript statements
  deriving (Eq, Ord, Show)

-- | Evaluate some raw JavaScript in the context of a given page
--
-- Returns either a deserialized Haskell type, or a human-readable string
-- describing any error that occurred. Possible errors include:
--
-- * Any exception in the given JavaScript
-- * Absence of any browser window currently viewing the page (since there's no
--   way to evaluate JavaScript without a JavaScript engine)
-- * Evaluation timeout (default is 1000 milliseconds, but can be overridden in
--   the timeout parameter to this function
-- * Invalid JSON response for the result type inferred (use 'JSON.Value' if you
--   don't know what shape of data you're waiting to receive).
--
-- Further caveats:
--
-- * JavaScript @undefined@ is translated to @null@ in the results
-- * 'JsBlock' inputs which don't explicitly return a value result in @null@
-- * Return types are limited to those which can be serialized via
--   [@JSON.stringify@](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify),
--   which does not work for cyclic objects (like @window@, @document@, and all
--   DOM nodes), and may fail to serialize some properties for other non-scalar
--   values. If you want to return a non-scalar value like a list or dictionary,
--   construct it explicitly yourself by copying from the fields of the object
--   you're interested in.
evaluateJs ::
  JSON.FromJSON a =>
  PageLocation {- ^ The location of the page in which to evaluate the JavaScript -} ->
  Maybe Int {- ^ An optional override for the default timeout of 1000 milliseconds -} ->
  JavaScript {- ^ The JavaScript to evaluate: either a 'JsExpression' or a 'JsBlock' -} ->
  IO (Either String a)
evaluateJs PageLocation{pageLocationPort = Last maybePort,
                        pageLocationPath = Last maybePath} timeout js =
  do result <- Req.runReq Req.defaultHttpConfig $
       Req.req Req.POST url body Req.lbsResponse
         (portOption <> timeoutOption <> exprOption)
     pure if Req.responseStatusCode result == 200
       then JSON.eitherDecode (Req.responseBody result)
       else Left (ByteString.unpack (Req.responseBody result))
  where
    url = pageUrl (fromMaybe "" maybePath)
    portOption = Req.port (maybe defaultPort (\(PagePort p) -> p) maybePort)

    body :: Req.ReqBodyLbs
    exprOption, timeoutOption :: Req.Option 'Req.Http
    timeoutOption = foldMap ("timeout" Req.=:) timeout
    (body, exprOption) = case js of
      JsExpression expr ->
        (Req.ReqBodyLbs "", "evaluate" Req.=: expr)
      JsBlock block ->
        (Req.ReqBodyLbs (ByteString.fromStrict (Text.encodeUtf8 block)),
         Req.queryFlag "evaluate")

-- | Given a page location and list of events, make a request to Myxine for the
-- event stream at that location and run the provided callback for each typed
-- event in the stream. If no list of events is specified (@Nothing@), every
-- event will be listened for. If the specific list of no events (@Just []@) is
-- specified, this function will sleep forever.
--
-- This blocks until the stream is closed by the server, or an exception is
-- encountered. This throws 'EventParseException' if the server sends an
-- unparseable event (this shouldn't happen in normal operation), and
-- 'Req.HttpException' if the underlying connection has an issue.
-- withEvents ::
--   PageLocation
--     {- ^ The location of the page to listen for events from -} ->
--   EventList
--     {- ^ The list of events for which to listen -} ->
--   (forall d. EventType d -> d -> [Target] -> IO ())
--     {- ^ The action to perform when each event happens -} ->
--   IO ()
-- withEvents _ (Just []) _ =
--   -- if the user requests no events, there is no corresponding Myxine API
--   -- request to handle this, so we just sleep forever
--   forever (threadDelay maxBound)
-- withEvents PageLocation{pageLocationPort = Last maybePort,
--                         pageLocationPath = Last maybePath} events perEvent =
--   do withStreamEvents url
--        (portOption <> eventParams)
--        \StreamEvent{eventId, eventType, eventData} -> do
--          targets <- either (throwIO . TargetParseException)
--                            pure (JSON.eitherDecode eventId)
--          withParsedEvent eventType eventData \case
--            Left Nothing -> throwIO (UnknownEventTypeException eventType)
--            Left (Just err) -> throwIO (EventDataParseException eventType err eventData)
--            Right (eventTy, properties) ->
--              perEvent eventTy properties targets
--   where
--     url = pageUrl (fromMaybe "" maybePath)
--     portOption = Req.port (maybe defaultPort (\(PagePort p) -> p) maybePort)

eventParams :: EventList -> Req.Option 'Req.Http
eventParams events = case events of
  AllEvents -> Req.queryFlag "events"
  SomeEvents es -> flip foldMap es
    \(Some e) -> "event" Req.=: ByteString.unpack (encodeEventType e)
