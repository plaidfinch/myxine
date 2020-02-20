{-# language BlockArguments, NamedFieldPuns, OverloadedStrings,
  ScopedTypeVariables, TypeApplications, LambdaCase #-}

module Myxine.Page
  ( -- * Creating Interactive Pages
  Page, runPage, waitPage, stopPage,
  -- * Manipulating Running Pages
  modifyPage, getPage, setPage, withPage
  -- * Specifying Page Locations
  , PageLocation, path, port
  ) where

import Data.Monoid
import Control.Monad
import Data.IORef
import Data.Maybe
import Control.Exception (SomeException, throwIO)
import qualified Control.Exception as Exception
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Concurrent

import Myxine.EventLoop
import Myxine.Handlers

-- | The options for connecting to the Myxine server. This is an opaque
-- 'Monoid': set options by combining 'port' and/or 'path' with '(<>)'.
data PageLocation
  = PageLocation
  { pageLocationPort :: Last Int
  , pageLocationPath :: Last Text
  } deriving (Eq, Ord, Show)

-- | Set the path to something other than the default of @/@.
path :: Text -> PageLocation
path p = mempty { pageLocationPath = Last (Just p) }

-- | Set the port to a non-default port. This is only necessary when Myxine is
-- running on a non-default port also.
port :: Int -> PageLocation
port p = mempty { pageLocationPort = Last (Just p) }

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

-- | A handle to a running page with internal state. To create a 'Page', use
-- 'runPage'. To interact with it further, see the rest of this module.
data Page state
  = Page
    { pageActions     :: !(Chan (Maybe (state -> IO state)))
    , pageFinished    :: !(MVar (Either SomeException state))
    , pageLocation    :: !PageLocation
    , pageEventThread :: !ThreadId }

-- | Run an interactive page, returning a handle 'Page' through which it can be
-- interacted. This function takes four arguments: a 'PageLocation' describing
-- which path and/or local port to connect to; an initial @state@ for the model
-- of the page; a set of 'Handlers' describing how to effectfully update the
-- state upon events that happen in the browser; and a view function which
-- renders a @state@ as an HTML 'ByteString' (how you do this is up to you) and
-- optionally a page title.
--
-- This function itself is non-blocking: it immediately kicks off threads to
-- start running the page. It will not throw exceptions by itself. All
-- exceptions thrown by page threads (such as issues with connecting to the
-- server) are deferred until a call to 'waitPage'.
runPage :: forall state.
  PageLocation
    {- ^ The location of the 'Page' ('port' and/or 'path') -} ->
  state
    {- ^ The initial @state@ of the 'Page' -} ->
  Handlers state
    {- ^ The set of event 'Handlers' for events in the page -} ->
  (state -> (Maybe Text, ByteString))
    {- ^ A function to draw the @state@ as an optional @<title>@ and @<body>@ contents -} ->
  IO (Page state)
    {- ^ A 'Page' handle to permit further interaction with the running page -}
runPage pageLocation@PageLocation{pageLocationPath, pageLocationPort}
        initialState handlers draw =
  do pageActions  <- newChan       -- channel for state-modifying actions
     pageFinished <- newEmptyMVar  -- signal for the page's shutdown

     -- Loop through all the events on the page, translating them to events
     pageEventThread <- forkIO $
       flip Exception.finally (writeChan pageActions Nothing) $  -- tell state thread to stop
         Exception.handle (putMVar pageFinished . Left) $  -- finished with exception
           Exception.handle @Exception.AsyncException (const (pure ())) $ -- don't track when the thread is killed
             do writeChan pageActions (Just redraw)
                withEvents
                  (fromMaybe defaultPort (getLast pageLocationPort))
                  (fromMaybe "" (getLast pageLocationPath))
                  (Just (handledEvents handlers))
                  \event properties targets ->
                    writeChan pageActions . Just $
                      redraw <=< handle handlers event properties targets

     -- Loop through all the actions, doing them
     _pageStateThread <- forkIO
       do state <- newIORef initialState  -- current state of the page
          Exception.handle (putMVar pageFinished . Left) $
            let loop = readChan pageActions >>= \case
                  Nothing -> putMVar pageFinished . Right =<< readIORef state
                  Just action ->
                    do writeIORef state =<< action =<< readIORef state
                       loop
            in loop

     pure (Page { pageActions, pageLocation, pageFinished, pageEventThread })

  where
    redraw :: state -> IO state
    redraw state =
      do let (title, body) = draw state
         sendUpdate
           (fromMaybe defaultPort (getLast pageLocationPort))
           (fromMaybe "" (getLast pageLocationPath))
           (Dynamic title body)
         pure state

-- | Wait for a 'Page' to finish executing and return its resultant 'state', or
-- re-throw any exception the page encountered.
waitPage :: Page state -> IO state
waitPage Page{pageFinished, pageEventThread} =
  do result <- takeMVar pageFinished
     killThread pageEventThread
     either throwIO pure result

-- | Politely request a 'Page' to shut down. This is non-blocking: to get the
-- final state of the 'Page', follow 'stopPage' with a call to 'waitPage'.
stopPage :: Page state -> IO ()
stopPage Page{pageActions} =
  writeChan pageActions Nothing

-- | Modify the state of the page with a pure function, and update the view in
-- the browser to reflect the new state.
modifyPage :: Page state -> (state -> state) -> IO ()
modifyPage page f = withPage page (pure . f)

-- | Set the state of the page to a particular value, and update the view in the
-- browser to reflect the new state.
setPage :: Page state -> state -> IO ()
setPage page = modifyPage page . const

-- | Get the current state of the page. Note that this function, while blocking
-- the thread which calls it, does not guarantee that the returned state is
-- fresh -- further updates may have happened by the time you act on the
-- returned state. That is, @getPage page >>= setPage page@ is not the same as
-- @modifyPage id@!
getPage :: Page state -> IO state
getPage page = do
  v <- newEmptyMVar
  withPage page \state ->
    do putMVar v state
       pure state
  takeMVar v

-- | Modify the state of the page, potentially doing arbitrary other effects in
-- the 'IO' monad, then re-draw the page to the browser. This is the primitive
-- upon which all other 'Page'-manipulating functions are built.
withPage :: Page state -> (state -> IO state) -> IO ()
withPage Page{pageActions} action =
  writeChan pageActions (Just action)
