{-|
Description : Direct one-to-one typed bindings to the Myxine server API

Usually, it makes the most sense to run a Myxine application using the
'Myxine.Page.Page' abstraction in the main module. However, this reactive
model-view-controller approach may not be appropriate for all needs. The
functions below are a one-to-one mapping to the API of the Myxine server.

Like the Myxine server API itself, this interface has a small surface area. You
can send a new page 'Update' using 'update', you can loop over all page
events using 'events', and you can evaluate raw JavaScript using
'evaluateJs'.
-}
module Myxine.Direct
  ( -- * Page locations on @localhost@
    PageLocation, pagePort, PagePort, pagePath, PagePath
    -- * Sending updates to pages and getting events from pages
  , Update(..), EventList(..), PageEvent(..), Target, tag, attribute
  , PageContent, pageBody, pageTitle, pageContentBody, pageContentTitle
  , update, events
    -- * Evaluating raw JavaScript in the context of a page
  , JavaScript(..), evaluateJs, JsException(..)
    -- * Exceptions thrown if the server misbehaves
  , ProtocolException(..)
    -- * The @Some@ existential
  , Some(..)
  , module Myxine.Event
  ) where

import Data.Maybe
import Control.Monad
import Data.Monoid
import Data.String
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad.IO.Class
import Control.Exception
import Data.Constraint
import Data.Some.Newtype (Some(..))
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import Data.IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Aeson as JSON
import qualified Network.HTTP.Req as Req
import qualified Text.URI as URI
import qualified Salve
import Data.Version (showVersion)

import Myxine.Internal.Event
import Myxine.Event
import Myxine.Target
import Paths_myxine_client (version)

-- | The view of a page, as rendered in the browser. Create page content with
-- 'pageBody' and 'pageTitle', and combine content using the 'Semigroup'
-- instance.
--
-- __Note:__ The 'Semigroup' instance for 'PageContent' takes the last specified
-- 'pageTitle' (if any), and concatenates in order each specified 'pageBody'.
data PageContent
  = PageContent
    { _pageContentBody  :: Text
    , _pageContentTitle :: Last Text
    } deriving (Eq, Ord, Show)

instance Semigroup PageContent where
  PageContent body title <> PageContent body' title' =
    PageContent (body <> body') (title <> title')

instance Monoid PageContent where
  mempty = PageContent mempty mempty

-- | Create a rendered 'PageContent' with an empty @title@ and the specified
-- text as its @body@.
pageBody :: Text -> PageContent
pageBody body = mempty { _pageContentBody = body }

-- | Create a rendered 'PageContent' with an empty @body@ and the specified
-- text as its @title@.
pageTitle :: Text -> PageContent
pageTitle title = mempty { _pageContentTitle = Last (Just title) }

-- | Get the rendered @body@ of a 'PageContent'.
pageContentBody :: PageContent -> Text
pageContentBody = _pageContentBody

-- | Get the @title@ of a 'PageContent'.
pageContentTitle :: PageContent -> Maybe Text
pageContentTitle = getLast . _pageContentTitle

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

-- TODO: Allow remote connections!
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

-- | A @PageEvent@ is an event that occurred in the browser: a triple of the
-- 'EventType', any associated properties of the event (this varies depending on
-- the event type), and the list of 'Target's of the event, in order from most
-- to least specific.
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
  do response <- Req.runReq Req.defaultHttpConfig $
       Req.req Req.POST url body Req.ignoreResponse options
     checkServerVersion response
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
      Dynamic (PageContent{_pageContentTitle = Last maybeTitle,
                           _pageContentBody = text}) ->
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

-- | An exception thrown by evaluating JavaScript. This may be a deserialization
-- error, or an error that occurred in the JavaScript runtime itself.
newtype JsException =
  JsException String
  deriving newtype (Eq, Ord, Show)
  deriving anyclass (Exception)

-- | Evaluate some raw JavaScript in the context of a given page.
--
-- Returns either a deserialized Haskell type, or throws a 'JsException'
-- containing a human-readable string describing any error that occurred.
--
-- Possible errors include:
--
-- Possible errors, which manifest as 'JsException's:
--
--   * Any exception in the given JavaScript
--
--   * Invalid JSON response for the result type inferred (use 'JSON.Value' if you
--   don't know what shape of data you're waiting to receive).
--
-- Further caveats:
--
--   * JavaScript @undefined@ is translated to @null@ in the results
--
--   * Return types are limited to those which can be serialized via
--   [JSON.stringify](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify),
--   which does not work for cyclic objects (like @window@, @document@, and all
--   DOM nodes), and may fail to serialize some properties for other non-scalar
--   values. If you want to return a non-scalar value like a list or dictionary,
--   construct it explicitly yourself by copying from the fields of the object
--   you're interested in.
--
--   * You're evaluating an arbitrary string as JavaScript, which means there
--   are no guarantees about type safety or purity.
--
--   * It is possible that you could break the Myxine server code running in
--   the page that makes it update properly, or hang the page by passing a
--   non-terminating piece of code.
--
--   * Any modifications you make to the DOM will be immediately overwritten on
--   the next re-draw of the page. Don't do this.
--
--   * If there are multiple browser windows pointed at the same page, and the
--   result of your query differs between them, it's nondeterministic which
--   result you get back.

evaluateJs ::
  JSON.FromJSON a =>
  PageLocation {- ^ The location of the page in which to evaluate the JavaScript -} ->
  JavaScript {- ^ The JavaScript to evaluate: either a 'JsExpression' or a 'JsBlock' -} ->
  IO a
evaluateJs PageLocation{pageLocationPort = Last maybePort,
                        pageLocationPath = Last maybePath} js =
  wrapCaughtReqException $
  do response <- Req.runReq
       Req.defaultHttpConfig { Req.httpConfigCheckResponse = \_ _ _ -> Nothing } $
         Req.req Req.POST url body Req.lbsResponse options
     checkServerVersion response
     if Req.responseStatusCode response == 200
       then either (throwIO . JsException) pure $
            JSON.eitherDecode (Req.responseBody response)
       else throwIO (JsException (ByteString.unpack (Req.responseBody response)))
  where
    url = pageUrl (fromMaybe "" maybePath)

    options =
      Req.port (maybe defaultPort (\(PagePort p) -> p) maybePort) <>
      Req.responseTimeout maxBound <>
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
-- create only one such "get next" action and to poll it repeatedly.
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
    {- ^ The location of the page to listen for events from. -} ->
  IO (EventList -> IO PageEvent)
    {- ^ An action which polls for the next event matching the given list, and
         blocks until such an event arrives. -}
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
           maybe (liftIO . throwIO . MyxineProtocolException $
                  "Invalid Content-Location:" <> show currentMoment)
                 pure
                 (urlAndOptions currentMoment eventList)
         response <- Req.req Req.GET url Req.NoReqBody Req.jsonResponse options
         liftIO $ checkServerVersion response
         let nextMoment =
               Text.decodeUtf8 <$>
                 Req.responseHeader response "Content-Location"
         -- If any async exception happens before here, we won't increment the
         -- moment. We make sure that if an exception happens during the
         -- increment or the return, we roll back the increment before
         -- re-throwing the exception, which ensures this function is safe to
         -- use inside an Async task (that is, cancellation won't skip events).
         liftIO $ catch @SomeException
           (do writeIORef moment nextMoment
               pure (Req.responseBody response))
           (\e -> do
               writeIORef moment currentMoment
               throwIO e)

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
          <> Req.responseTimeout maxBound  -- important, because long-polling!

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
    Req.JsonHttpException message -> throwIO (MyxineProtocolException message)

-- | If the response from the server cannot be processed appropriately, this
-- exception is thrown. This should never happen in ordinary circumstances; if
-- it does, your version of the client library may mismatch the version of the
-- Myxine server you are running, or there may be a bug in the Myxine server or
-- this library.
--
-- If you encounter this exception in the wild, please file a bug report at
-- <https://github.com/GaloisInc/myxine/issues/new>. Thanks!
data ProtocolException
  = MyxineProtocolException String
  | MyxineServerVersionClashException Salve.Version
  | MyxineUnknownServerException
  deriving stock (Eq, Ord)
  deriving anyclass (Exception)

-- | The supported versions of the Myxine server for this version of the client
-- library. This needs to be bumped whenever the library is updated to match a
-- new server release.
supportedServers :: Salve.Constraint
supportedServers = Salve.unsafeParseConstraint ">=0.2.0 <0.3.0"

-- | Check a response to ensure that it originated from a server which describes
-- itself as being the correct server version for this client library. If this
-- check fails, throw an exception; otherwise, do nothing.
checkServerVersion :: Req.HttpResponse response => response -> IO ()
checkServerVersion response =
  case Data.ByteString.Char8.split '/' <$> Req.responseHeader response "server" of
    Nothing -> throwIO MyxineUnknownServerException
    Just ["myxine", versionString] ->
      case Salve.parseVersion (Data.ByteString.Char8.unpack (versionString <> ".0")) of
        Nothing -> throwIO MyxineUnknownServerException
        Just serverVersion ->
          when (not (Salve.satisfiesConstraint supportedServers serverVersion)) $
          throwIO (MyxineServerVersionClashException serverVersion)
    _ -> throwIO MyxineUnknownServerException

instance Show ProtocolException where
  show MyxineUnknownServerException =
       "*** Refusing to connect to a server that doesn't self-identify as a Myxine server.\n"
    <> "Ensure that the address and port are correct and try again."
  show (MyxineServerVersionClashException serverVersion) =
       "*** Myxine client/server version mismatch: myxine-client library " <> showVersion version
    <> " is incompatible with myxine server version " <> Salve.renderVersion serverVersion <> ".\n"
    <> "Either connect to a server in the compatible range of " <> Salve.renderConstraint supportedServers <> ","
    <> " or update this application to use a compatible client library version."
  show (MyxineProtocolException details) =
       "*** Myxine client panic: Failed to process event!\n"
    <> "This likely means there is a bug in the Myxine server or client library.\n"
    <> "Please file a bug report at: " <> bugReportURL <> ".\n"
    <> "Please include the version of this library (" <> showVersion version <> "), "
    <> "and the following error message: \n" <> details
    where
      bugReportURL :: String
      bugReportURL = "https://github.com/GaloisInc/myxine/issues/new"
