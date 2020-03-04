module Myxine.Handlers
  ( Handlers
  , on
  , handle
  , handledEvents
  , HandlerOption
  , Freshness(..)
  , tagIs
  , attrIs
  , freshEvent
  , Propagation(..)
  ) where

import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Hashable
import Data.Dependent.Map (DMap, Some)
import qualified Data.Dependent.Map as DMap
import GHC.Generics

import Myxine.Event
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
on ::
  EventType props ->
  HandlerOption ->
  (props -> model -> IO (Propagation, model)) ->
  Handlers model
on event (HandlerOption eventFacts) h =
  Handlers . DMap.singleton event . PerEventHandlers $
    ConjMap.insert eventFacts h mempty
{-# INLINE on #-}

newtype HandlerOption
  = HandlerOption [EventFact]
  deriving newtype (Semigroup, Monoid)

-- | A 'HandlerOption' specifying that the target must have the HTML tag given;
-- otherwise, this handler will not fire.
tagIs :: Text -> HandlerOption
tagIs t = HandlerOption [TargetFact (HasTag (Text.toLower t))]

-- | A 'HandlerOption' specifying that the target must have the HTML attribute
-- given, with the exact value specified; otherwise, this handler will not fire.
attrIs :: Text -> Text -> HandlerOption
attrIs a v = HandlerOption [TargetFact (AttributeEquals a v)]

-- | A 'HandlerOption' specifying that the event must have originated from a
-- browser view that is "freshEvent": that is, the browser's view exactly matches the
-- current model input to the handler. If this option is specified, the affected
-- handler will not fire when an event originated from an out-of-date view.
--
-- Why should this matter? The browser view can momentarily get out of sync due
-- to lag between events and their corresponding updates to the view. If an
-- event triggers a view update, but is immediately followed by another event
-- before the page can be updated, we need to decide what to do with this event.
-- Usually the right option is to process it as usual. However, this can lead to
-- odd behavior when the first event triggers a shift in view that would make
-- the second event impossible if it happened slightly later.
--
-- For instance, consider a button which, when clicked, prints a document and
-- also disappears from view. Without the 'fresh' 'HandlerOption', there would
-- be a possibility that clicking the button very fast would print two copies of
-- the document. On the other hand, consider an application that responds to
-- mouse movement by moving a paintbrush to match the cursor's location. Because
-- mouse movement is a high-frequency event, many of these updates could occur
-- before the page is re-drawn. In this case, it is undesirable to drop stale
-- events, because this would constitute losing desired user input and failing
-- to record some mouse movements on the canvas. For these reasons, the policy
-- to handle stale events must be an application-level concern, and is therefore
-- exposed as a 'HandlerOption'.
freshEvent :: HandlerOption
freshEvent = HandlerOption [FreshEvent]

-- | An enum describing whether an event pertains to a DOM that is outdated
-- relative to the current model
data Freshness
  = Fresh  -- ^ The event was the first to happen since the DOM was updated
  | Stale  -- ^ The event was not the first to happen since an update

-- | Dispatch all the event handler callbacks for a given event type and its
-- corresponding data. Event handlers for this event type will be called in the
-- order they were registered (left to right) with the result of the previous
-- handler fed as the input to the next one.
handle ::
  Handlers model ->
  Freshness ->
  EventType props ->
  props ->
  [Target] ->
  model ->
  IO model
handle (Handlers allHandlers) freshness event props path model =
  let PerEventHandlers targetMap =
        fromMaybe mempty (DMap.lookup event allHandlers)
      addFreshness = case freshness of
        Fresh -> (FreshEvent :)
        _ -> id
      facts = map (addFreshness . map TargetFact . targetFacts) path
      handlers = map (flip ConjMap.lookup targetMap) facts
  in processHandlers handlers model
  where
    processHandlers [                  ] m = pure m
    processHandlers ([      ] : parents) m = processHandlers parents m
    processHandlers ((h : hs) : parents) m =
      do (propagation, m') <- h props m
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

-- | A handler for a single event type with associated data @props@.
newtype PerEventHandlers model props
  = PerEventHandlers (ConjMap EventFact (props -> model -> IO (Propagation, model)))
  deriving newtype (Semigroup, Monoid)

-- | Individual facts about an event which may be used to filter events to be
-- directed only to particular handlers. Each handler is keyed to a conjunction
-- of such facts.
data EventFact
  = FreshEvent
  | TargetFact TargetFact
  deriving (Eq, Ord, Show, Generic, Hashable)
