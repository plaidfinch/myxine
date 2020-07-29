{-| Description : Declarative handlers for page events (usually not necessary
  unless you're making an alternative to the 'Myxine.Reactive' builder)

In order to react to user events in the browser, we need to specify what effect
each event of interest should have on the model in our 'Myxine.Page'. To do
this, 'Myxine.runPage' asks that we construct up-front a set of 'Handlers'
describing this.

'Handlers' is a 'Monoid': the 'mempty' 'Handlers' listens to no events.
Singleton 'Handlers' can be created using the 'onEvent' function, and they can
be joined together using '<>'.

This module is useful when you are building your own page event handling
abstraction, for instance, if "Myxine.Reactive" isn't right for your purposes.
However, it is not necessary to usee this module directly if you are buliding a
reactive page using that high-level abstraction.
-}

module Myxine.Handlers
  ( Handlers
  , onEvent
  , handle
  , handledEvents
  , TargetFact
  , tagIs
  , attrIs
  , window
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
-- The provided callback will be given the properties @props@ of this particular
-- event, and the current @model@ of a page. It has the option to do arbitrary
-- 'IO', and to return a possibly-changed @model@. It also must specify whether
-- or not the event should continue to propagate outwards to other handlers, by
-- giving a 'Propagation' (either 'Bubble' or 'Stop').
--
-- The callback will only be invoked when an event occurs which matches the
-- conjunction of the specified list of 'TargetFact's. For instance, to
-- constrain a handler to only events on @<div>@ elements with @class="foo"@, we
-- would use the 'TargetFact' @[tagIs "div", "class" `attrIs` "foo"]@.
--
-- Notice that each variant of 'EventType' has a type-level index describing
-- what kind of data is carried by events of that type. This means that, for
-- instance, if you want to handle a 'Click' event, which has the type
-- 'EventType MouseEvent', your event handler as created by 'Myxine.on' will be
-- given access to a 'MouseEvent' data structure when it is invoked. That is to
-- say:
--
-- @
-- 'onEvent' 'Click'
--    ['tagIs' "div", "class" \`'attrIs'\` "foo"]
--    (\\properties@'MouseEvent'{} model ->
--       do print properties
--          print model
--          pure (Bubble, model))
--   :: 'Show' model => 'Handlers' model
-- @
--
-- A full listing of all available 'EventType's and their corresponding property
-- records can be found in the below section on [types and properties of
-- events](#Types).
onEvent ::
  EventType props ->
  [TargetFact] ->
  (props -> model -> IO (Propagation, model)) ->
  Handlers model
onEvent event eventFacts h =
  Handlers . DMap.singleton event . PerEventHandlers $
    ConjMap.insert eventFacts h mempty
{-# INLINE onEvent #-}

-- | A 'TargetFact' specifying that the target must have the HTML tag given;
-- otherwise, this handler will not fire.
tagIs :: Text -> TargetFact
tagIs t = HasTag (Text.toLower t)

-- | A 'TargetFact' specifying that the target must have the HTML attribute
-- given, with the exact value specified; otherwise, this handler will not fire.
attrIs :: Text -> Text -> TargetFact
attrIs a v = AttributeEquals a v

-- | A 'TargetFact' specifying that the target must be the root DOM element,
-- that is, the @window@ object.
window :: TargetFact
window = Window

-- | Dispatch all the event handler callbacks for a given event type and its
-- corresponding data.
--
-- Event handlers for this event type will be called in the order they were
-- registered (left to right) with the result of the previous handler fed as the
-- input to the next one. If any event handler in the chain returns 'Stop', then
-- propagation stops at the current 'Target' for that handler.
handle :: Handlers model -> PageEvent -> model -> IO model
handle (Handlers allHandlers) PageEvent{event, properties, targets} model =
  let PerEventHandlers targetMap =
        fromMaybe mempty (DMap.lookup event allHandlers)
      facts = map targetFacts targets ++ [[Window]]
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
{-# INLINE handle #-}

-- TODO: add zoom combinator!

-- | Get a list of all the events which are handled by these handlers.
handledEvents :: Handlers model -> [Some EventType]
handledEvents (Handlers handlers) = DMap.keys handlers

-- | A set of handlers for events, possibly empty. Create new 'Handlers' using
-- 'onEvent', and combine 'Handlers' together using their 'Monoid' instance.
newtype Handlers model
  = Handlers (DMap EventType (PerEventHandlers model))

instance Semigroup (Handlers model) where
  Handlers hs <> Handlers hs' =
    Handlers (DMap.unionWithKey (const (<>)) hs hs')

instance Monoid (Handlers model) where
  mempty = Handlers mempty

-- | Indicator for whether an event should continue to be triggered on parent
-- elements in the path. An event handler can signal that it wishes the event to
-- stop propagating by returning 'Stop'.
data Propagation
  = Bubble  -- ^ Continue to trigger the event on parent elements
  | Stop    -- ^ Continue to trigger the event for all handlers of this element,
            -- but stop before triggering it on any parent elements
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Semigroup Propagation where
  l <> r | l > r = l
         | otherwise = r

instance Monoid Propagation where
  mempty = Bubble

-- | A handler for a single event type with associated data @props@.
newtype PerEventHandlers model props
  = PerEventHandlers (ConjMap TargetFact (props -> model -> IO (Propagation, model)))
  deriving newtype (Semigroup, Monoid)

-- a pure function from the current state of the page's
--         model to a rendered HTML view of the page in its entirety, and the new
--         set of 'Handlers' for page events. A handler can modify the model of the
--         page, and perform arbitrary 'IO' actions. After each all pertinent
--         handlers for an event are dispatched, the page is re-rendered to the
--         browser. The @drawAndHandle@ function is run in a 'WithinPage' context,
--         which means that the 'eval' and 'evalBlock' functions may be used within
--         handlers to evaluate JavaScript within the context of the current page.

--       This library doesn't bind you into a specific approach for HTML
--       generation: you can construct some 'Direct.PageContent' from
--       'Data.Text.Text' using 'pageTitle' and 'pageBody', and you can build a set
--       of 'Handlers' by combining calls to 'Myxine.Handlers.onEvent' with the
--       'Monoid' instance for 'Handlers'.

--       __However__, the above manual method for constructing page logic can get
--       messy as pages get more complex. To solve this issue, this package also
--       provides the optional 'Reactive' DSL for describing pages by
--       /interleaving/ content and event handling:
