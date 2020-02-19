module Myxine.Handlers (Handlers, on, handle, handledEvents) where

import Data.Maybe
import Data.Dependent.Map (DMap, Some)
import qualified Data.Dependent.Map as DMap

import Myxine.Event
import Myxine.Target

-- | Create a handler for a specific event type by specifying the type of event
-- and the monadic callback to be invoked when the event occurs. The provided
-- callback will be given the data corresponding to this event, as well as a
-- list of 'Target's on which the event fired, in order from most to least
-- specific.
on :: EventType props -> (props -> [Target] -> state -> IO state) -> Handlers state
on event h = Handlers (DMap.singleton event (Handler h))
{-# INLINE on #-}

-- | Dispatch all the event handler callbacks for a given event type and its
-- corresponding data. Event handlers for this event type will be called in the
-- order they were registered (left to right) with the result of the previous
-- handler fed as the input to the next one.
handle :: Handlers state -> EventType props -> props -> [Target] -> state -> IO state
handle (Handlers allHandlers) event props path state =
  let Handler handler =
        fromMaybe (Handler (const (const (const (pure state)))))
                  (DMap.lookup event allHandlers)
  in handler props path state
{-# INLINE handle #-}

-- | Get a list of all the events which are handled by these handlers.
handledEvents :: Handlers state -> [Some EventType]
handledEvents (Handlers handlers) = DMap.keys handlers

-- | A set of handlers for events, possibly empty. Create new 'Handlers' using
-- 'on', and combine 'Handlers' together using their 'Semigroup' instance.
newtype Handlers state
  = Handlers (DMap EventType (Handler state))

instance Semigroup (Handlers state) where
  Handlers hs <> Handlers hs' =
    Handlers (DMap.unionWithKey (const (<>)) hs hs')

-- | A handler for a single event type with associated data @props@.
newtype Handler state props
  = Handler (props -> [Target] -> state -> IO state)

instance Semigroup (Handler state props) where
  Handler h <> Handler g =
    Handler (\props p a -> h props p a >>= g props p)

instance Monoid (Handler state props) where
  mempty = Handler (\_ _ a -> pure a)
