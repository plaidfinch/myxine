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
    -- * Sending updates to pages and getting events from pages
  , Update(..), EventList(..), PageEvent(..)
  , PageContent, pageBody, pageTitle, update, events
    -- * Evaluating raw JavaScript in the context of a page
  , JavaScript(..), evaluateJs
    -- * The @Some@ existential
  , Some(..)
  ) where

import Data.Maybe
import Data.Monoid
import Data.String
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad.IO.Class
import Control.Exception
import Data.Constraint
import Data.Dependent.Map (Some(..))
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import Data.IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Aeson as JSON
import qualified Network.HTTP.Req as Req
import qualified Text.URI as URI

import Myxine.Event
import Myxine.Target

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

host :: Text
host = "localhost"

-- | Compute the localhost 'Req.Url' of a given path, allowing for dynamic
-- appearances of @/@ in the URL.
pageUrl :: PagePath -> Req.Url 'Req.Http
pageUrl (PagePath p) =
  foldl' (Req./:) (Req.http host) (Text.split ('/' ==) p)

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

instance Show PageEvent where
  showsPrec d PageEvent{event, properties, targets} =
    case eventPropertiesDict event of
      Dict -> showParen (d > 10) $
        showString "PageEvent {event = " .
        showsPrec 0 event .
        showString ", properties = " .
        showsPrec 0 properties .
        showString ", targets = " .
        showsPrec 0 targets .
        showString "}"

instance JSON.FromJSON PageEvent where
  parseJSON = JSON.withObject "PageEvent" \o ->
    do eventName <- o JSON..: "event"
       Some event <-
         flip maybe pure
          (fail ("Unrecognized event: " <> Text.unpack eventName))
          (decodeSomeEventType eventName)
       Dict <- pure (eventPropertiesDict event)
       properties <- o JSON..: "properties"
       targets <- o JSON..: "targets"
       pure (PageEvent{event, properties, targets})

-- | A list of event types to listen for: either all events, or a specific list
-- of events.
data EventList
  = AllEvents  -- ^ Listen for all events
  | SomeEvents (NonEmpty (Some EventType))  -- ^ Listen only for these events
  deriving (Eq, Ord, Show)

-- | Send a full-page update to Myxine at a particular port and path. An
-- 'Update' is either a 'Dynamic' page body with an optional title, or a
-- 'Static' file with a particular Content-Type.
update ::
  PageLocation
    {- ^ The location of the page to update -} ->
  Update
    {- ^ The new content of the page to display -} ->
  IO ()
update PageLocation{pageLocationPort = Last maybePort,
                        pageLocationPath = Last maybePath} updateContent =
  wrapCaughtReqException $
  do _ <- Req.runReq Req.defaultHttpConfig $
       Req.req Req.POST url body Req.ignoreResponse options
     pure ()
  where
    url = pageUrl (fromMaybe "" maybePath)

    options =
      Req.port (maybe defaultPort (\(PagePort p) -> p) maybePort) <>
      Req.responseTimeout maxBound <>
      updateOptions

    body :: Req.ReqBodyLbs
    updateOptions :: Req.Option 'Req.Http
    (body, updateOptions) = case updateContent of
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
  wrapCaughtReqException $
  do result <- Req.runReq Req.defaultHttpConfig $
       Req.req Req.POST url body Req.lbsResponse options
     pure if Req.responseStatusCode result == 200
       then JSON.eitherDecode (Req.responseBody result)
       else Left (ByteString.unpack (Req.responseBody result))
  where
    url = pageUrl (fromMaybe "" maybePath)

    options =
      Req.port (maybe defaultPort (\(PagePort p) -> p) maybePort) <>
      Req.responseTimeout maxBound <>
      foldMap ("timeout" Req.=:) timeout <>
      exprOption

    body :: Req.ReqBodyLbs
    exprOption :: Req.Option 'Req.Http
    (body, exprOption) = case js of
      JsExpression expr ->
        (Req.ReqBodyLbs "", "evaluate" Req.=: expr)
      JsBlock block ->
        (Req.ReqBodyLbs (ByteString.fromStrict (Text.encodeUtf8 block)),
         Req.queryFlag "evaluate")

-- | Given a page location and list of events, create an IO action that acts as
-- a "stream" of sequential events matching the event list. The state maintained
-- within the stream is used to coordinate with the server to return a
-- sequential event from each poll of the stream. It is strongly recommended to
-- create one such "get next" action and poll it repeatedly. The latter, unlike
-- the former, might skip arbitrary numbers of events when repeatedly invoked!
--
-- Calls to the "get next" action returned will block until the next event
-- matching the given description is available. Provided that the "get next"
-- action is polled with sufficient frequency, no events will be missed, as the
-- server maintains an internal fixed-size buffer of events to distribute to
-- lagging clients. However, significantly lagging clients may observe dropped
-- events. It is therefore best practice to eagerly collect events in a separate
-- tight-looping thread and buffer them client-side.
events ::
  PageLocation
    {- ^ The location of the page to listen for events from -} ->
  IO (EventList -> IO PageEvent)
events PageLocation{pageLocationPort = Last maybePort,
                    pageLocationPath = Last maybePath} =
  do moment <- newIORef Nothing
     pure (\eventList -> wrapCaughtReqException $
            Req.runReq Req.defaultHttpConfig (go moment eventList))
  where
    go :: IORef (Maybe Text) -> EventList -> Req.Req PageEvent
    go moment eventList =
      do currentMoment <- liftIO (readIORef moment)
         (url, options) <-
           maybe (liftIO . throwIO . ProtocolException $
                  "Invalid Content-Location:" <> show currentMoment)
                 pure
                 (urlAndOptions currentMoment eventList)
         response <- Req.req Req.GET url Req.NoReqBody Req.jsonResponse options
         let nextMoment =
               Text.decodeUtf8 <$>
                 Req.responseHeader response "Content-Location"
         liftIO (writeIORef moment nextMoment)
         pure (Req.responseBody response)

    urlAndOptions ::
      Maybe Text ->
      EventList ->
      Maybe (Req.Url 'Req.Http, Req.Option 'Req.Http)
    urlAndOptions maybeMoment eventList =
      case maybeMoment of
        Nothing ->
          pure (pageUrl (fromMaybe "" maybePath), fixedOptions)
        Just moment ->
          -- Parse the URI given and make it absolute to the server
          do URI.URI { uriScheme,
                       uriAuthority,
                       uriPath,
                       uriQuery,
                       uriFragment } <- URI.mkURI moment
             -- Fill in the default scheme and authority if absent
             let uriScheme' = Just $
                   maybe (fromJust (URI.mkScheme "http")) id uriScheme
                 uriAuthority' = Right $
                   either (const $
                           URI.Authority { authUserInfo = Nothing
                                         , authHost = fromJust (URI.mkHost host)
                                         , authPort = Nothing })
                          id
                          uriAuthority
                 uri = URI.URI { uriScheme = uriScheme'
                               , uriAuthority = uriAuthority'
                               , uriPath
                               , uriQuery
                               , uriFragment }
             -- Parse into Req's way of seeing the world
             (url, momentOption) <- Req.useHttpURI uri
             pure (url, fixedOptions <> momentOption)
      where
        fixedOptions :: Req.Option 'Req.Http
        fixedOptions =
          portOption <> eventParams <> Req.queryFlag "next"

        portOption :: Req.Option 'Req.Http
        portOption = Req.port (maybe defaultPort (\(PagePort p) -> p) maybePort)

        eventParams :: Req.Option 'Req.Http
        eventParams = case eventList of
          AllEvents -> Req.queryFlag "events"
          SomeEvents es -> flip foldMap es
            \(Some e) -> "event" Req.=: ByteString.unpack (encodeEventType e)

wrapCaughtReqException :: IO a -> IO a
wrapCaughtReqException action =
  catch @Req.HttpException action $
  \case
    Req.VanillaHttpException e -> throwIO e
    Req.JsonHttpException message -> throwIO (ProtocolException message)

-- | If the response from the server cannot be processed appropriately, this
-- exception is thrown. This should never happen in ordinary circumstances; if
-- it does, your version of the client library may mismatch the version of the
-- Myxine server you are running, or there may be a bug in the Myxine server or
-- this library.
--
-- If you encounter this exception in the wild, please file a bug report at
-- <https://github.com/GaloisInc/myxine/issues/new>. Thanks!
newtype ProtocolException
  = ProtocolException String
  deriving stock (Eq, Ord)
  deriving anyclass (Exception)

instance Show ProtocolException where
  show (ProtocolException details) =
       "*** Myxine client panic: Failed to process event! This means one of:\n\n"
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
