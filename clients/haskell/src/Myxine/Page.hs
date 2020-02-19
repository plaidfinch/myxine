{-# language BlockArguments, NamedFieldPuns, OverloadedStrings,
  ScopedTypeVariables, TypeApplications #-}

module Myxine.Page where

import Data.Monoid
import Data.Maybe
import Control.Exception (Exception, SomeException)
import qualified Control.Exception as Exception
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Network.HTTP.Req as Req

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

data Page state
  = Page
    { pageActions  :: Chan (Maybe (state -> IO state))
    , pageFinished :: MVar (Maybe SomeException)
    , pageLocation :: PageLocation }

runPage :: forall state.
  PageLocation -> state -> Handlers state -> (state -> ByteString) -> IO (Page state)
runPage pageLocation@PageLocation{pageLocationPath, pageLocationPort}
        initialState handlers draw =
  do pageActions <- newChan
     pageFinished <- newEmptyMVar
     tid <- forkIO $
       Exception.handle (putMVar pageFinished . Just)  -- finished with exception
       do redraw initialState
          withEvents
            (fromMaybe defaultPort (getLast pageLocationPort))
            (fromMaybe "" (getLast pageLocationPath))
            (handledEvents handlers)
            \event properties targets ->
              writeChan pageActions $ Just \state ->
                do state' <- handle handlers event properties targets state
                   redraw state'
                   pure state'
          putMVar pageFinished Nothing  -- finished normally
     pure (Page { pageActions, pageLocation, pageFinished })
  where
    redraw :: state -> IO ()
    redraw state = _ draw

withPage :: Page state -> (state -> IO state) -> IO ()
withPage Page{pageActions} action =
  writeChan pageActions (Just action)

modifyPage :: Page state -> (state -> state) -> IO ()
modifyPage page f = withPage page (pure . f)

setPage :: Page state -> state -> IO ()
setPage page = modifyPage page . const

getPage :: Page state -> IO state
getPage page = do
  v <- newEmptyMVar
  withPage page \state ->
    do putMVar v state
       pure state
  takeMVar v
