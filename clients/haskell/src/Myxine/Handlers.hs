module Myxine.Handlers (Handlers, on, handle, handledEvents) where

import Data.Maybe
import Data.Dependent.Map (DMap, Some)
import qualified Data.Dependent.Map as DMap

import Myxine.Event
import Myxine.Target

-- | Create a handler for a specific event type by specifying the type of event
-- and the monadic callback to be invoked when the event occurs.
--
-- The provided callback will be given the @'EventType' props@ of the event, the
-- properties @props@ of this particular event, a list of 'Target's on which the
-- event fired, in order from most to least specific, and the current @model@ of
-- a page. It has the option to do arbitrary 'IO', and to return a
-- possibly-changed @model@.
--
-- Notice that each variant of 'EventType' has a type-level index describing
-- what kind of data is carried by events of that type. This means that, for
-- instance, if you want to handle a 'Click' event, which has the type
-- 'EventType MouseEvent', your event handler as created by 'on' will be given
-- access to a 'MouseEvent' data structure when it is invoked. That is to say:
--
-- @
-- 'on' 'Click' (\properties@'MouseEvent'{} targets model ->
--                 do print properties
--                    print targets
--                    print model)
--   :: 'Show' model => 'Handlers' model
-- @
--
-- A full listing of all available 'EventType's and their corresponding property
-- records can be found in the below section on [types and properties of
-- events](#Types).
on :: EventType props -> (props -> [Target] -> model -> IO model) -> Handlers model
on event h = Handlers (DMap.singleton event (Handler h))
{-# INLINE on #-}

-- | Dispatch all the event handler callbacks for a given event type and its
-- corresponding data. Event handlers for this event type will be called in the
-- order they were registered (left to right) with the result of the previous
-- handler fed as the input to the next one.
handle :: Handlers model -> EventType props -> props -> [Target] -> model -> IO model
handle (Handlers allHandlers) event props path model =
  let Handler handler =
        fromMaybe (Handler (const (const (const (pure model)))))
                  (DMap.lookup event allHandlers)
  in handler props path model
{-# INLINE handle #-}

-- | Get a list of all the events which are handled by these handlers.
handledEvents :: Handlers model -> [Some EventType]
handledEvents (Handlers handlers) = DMap.keys handlers

-- | A set of handlers for events, possibly empty. Create new 'Handlers' using
-- 'on', and combine 'Handlers' together using their 'Monoid' instance.
newtype Handlers model
  = Handlers (DMap EventType (Handler model))

instance Semigroup (Handlers model) where
  Handlers hs <> Handlers hs' =
    Handlers (DMap.unionWithKey (const (<>)) hs hs')

instance Monoid (Handlers model) where
  mempty = Handlers mempty

-- | A handler for a single event type with associated data @props@.
newtype Handler model props
  = Handler (props -> [Target] -> model -> IO model)

instance Semigroup (Handler model props) where
  Handler h <> Handler g =
    Handler (\props p a -> h props p a >>= g props p)

instance Monoid (Handler model props) where
  mempty = Handler (\_ _ a -> pure a)
