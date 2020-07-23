module Myxine.Handlers
  ( Handlers
  , onEvent
  , handle
  , handledEvents
  , HandlerOption
  , tagIs
  , attrIs
  , Propagation(..)
  ) where

import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Myxine.Event
import Myxine.Direct
import Myxine.Target
import Myxine.ConjMap (ConjMap)
import qualified Myxine.ConjMap as ConjMap

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
onEvent ::
  EventType props ->
  HandlerOption ->
  (props -> model -> IO (Propagation, model)) ->
  Handlers model
onEvent event (HandlerOption eventFacts) h =
  Handlers . DMap.singleton event . PerEventHandlers $
    ConjMap.insert eventFacts h mempty
{-# INLINE onEvent #-}

newtype HandlerOption
  = HandlerOption [TargetFact]
  deriving newtype (Semigroup, Monoid)

-- | A 'HandlerOption' specifying that the target must have the HTML tag given;
-- otherwise, this handler will not fire.
tagIs :: Text -> HandlerOption
tagIs t = HandlerOption [HasTag (Text.toLower t)]

-- | A 'HandlerOption' specifying that the target must have the HTML attribute
-- given, with the exact value specified; otherwise, this handler will not fire.
attrIs :: Text -> Text -> HandlerOption
attrIs a v = HandlerOption [AttributeEquals a v]

-- | Dispatch all the event handler callbacks for a given event type and its
-- corresponding data. Event handlers for this event type will be called in the
-- order they were registered (left to right) with the result of the previous
-- handler fed as the input to the next one.
handle ::
  Handlers model ->
  PageEvent ->
  model ->
  IO model
handle (Handlers allHandlers) PageEvent{event, properties, targets} model =
  let PerEventHandlers targetMap =
        fromMaybe mempty (DMap.lookup event allHandlers)
      facts = map targetFacts targets
      handlers = map (flip ConjMap.lookup targetMap) facts
  in processHandlers handlers model
  where
    processHandlers [                  ] m = pure m
    processHandlers ([      ] : parents) m = processHandlers parents m
    processHandlers ((h : hs) : parents) m =
      do (propagation, m') <- h properties m
         case propagation of
           Bubble -> processHandlers (hs : parents) m'
           Stop   -> processHandlers (hs : [     ]) m'
           StopImmediately -> pure m'
{-# INLINE handle #-}

-- | Get a list of all the events which are handled by these handlers.
handledEvents :: Handlers model -> [Some EventType]
handledEvents (Handlers handlers) = DMap.keys handlers

-- | A set of handlers for events, possibly empty. Create new 'Handlers' using
-- 'on', and combine 'Handlers' together using their 'Monoid' instance.
newtype Handlers model
  = Handlers (DMap EventType (PerEventHandlers model))

instance Semigroup (Handlers model) where
  Handlers hs <> Handlers hs' =
    Handlers (DMap.unionWithKey (const (<>)) hs hs')

instance Monoid (Handlers model) where
  mempty = Handlers mempty

-- | Indicator for whether an event should continue to be triggered on parent
-- elements in the path
data Propagation
  = Bubble  -- ^ Continue to trigger the event on parent elements
  | Stop    -- ^ Continue to trigger the event for all handlers of this element,
            -- but stop before triggering it on any parent elements
  | StopImmediately  -- ^ Do not trigger any other event handlers
  deriving (Eq, Ord, Show)

instance Semigroup Propagation where
  l <> r | l > r = l
         | otherwise = r

instance Monoid Propagation where
  mempty = Bubble

-- | A handler for a single event type with associated data @props@.
newtype PerEventHandlers model props
  = PerEventHandlers (ConjMap TargetFact (props -> model -> IO (Propagation, model)))
  deriving newtype (Semigroup, Monoid)
