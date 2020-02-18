{-# language GADTs, DataKinds, DuplicateRecordFields, RankNTypes, StrictData,
    ScopedTypeVariables, BlockArguments, KindSignatures, TemplateHaskell,
    OverloadedStrings, DerivingStrategies, DerivingVia, StandaloneDeriving,
    DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Myxine
  (
  -- * Types of Events
  EventType(..)

  -- * Handling Events
  , Handlers
  , on
  , handle
  , Target
  , tagName
  , attribute

  -- * Parsing Events
  , withParsedEvent

  -- * Interfaces for Events

  -- | Each variant of 'EventType' has a type-level index describing what kind
  -- of data is carried by events of that type. Note the type of 'on':
  --
  -- @
  -- 'on' :: 'EventType' d -> (d -> ['Target'] -> a -> m b) -> 'Handlers' m a b
  -- @
  --
  -- This means that, for instance, if you want to handle a 'Click' event, which
  -- has the type 'EventType MouseEvent', your event handler as created by 'on'
  -- will be given access to a 'MouseEvent' data structure when it is invoked.
  -- That is to say:
  --
  -- @
  -- 'on' 'Click' :: ('MouseEvent' -> ['Target'] -> a -> m b) -> 'Handlers' m a b
  -- @
  --
  -- These types are automatically generated from Myxine's master specification
  -- of supported events and interfaces, so they will always match those
  -- supported by the version of Myxine corresponding to the version of this
  -- library. However, Template Haskell does not allow programmatic generation
  -- of Haddock documentation, so we can't put proper inline documentation
  -- below.
  --
  -- To aid in your reference, note that the name of each type below exactly
  -- match to the browser's name for events of that interface, and the names of
  -- each interface's properties exactly match the browser's names for them,
  -- except in the cases where those names are reserved keywords in Haskell. In
  -- those cases, we prepend the name of the interface (for instance, we use the
  -- property name @inputData@ instead of @data@).
  --
  -- For more details on the meaning of each type below and its fields, refer to
  -- Myxine's documentation and/or the [MDN web API documentation for events and
  -- their interfaces](https://developer.mozilla.org/docs/Web/Events).
  , module Event
  ) where

import Data.Text (Text)
import qualified Data.Aeson as JSON
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (ByteString)
import Data.Dependent.Map (DMap, Some(..))
import qualified Data.Dependent.Map as DMap
import Data.Maybe
import GHC.Generics

import Myxine.Event
import qualified Myxine.Event as Event
  hiding (decodeSomeEventType, decodeEventProperties, EventType(..))

-- | Create a handler for a specific event type by specifying the type of event
-- and the monadic callback to be invoked when the event occurs. The provided
-- callback will be given the data corresponding to this event, as well as a
-- list of 'Target's on which the event fired, in order from most to least
-- specific.
on :: EventType d -> (d -> [Target] -> a -> m a) -> Handlers m a
on event h = Handlers (DMap.singleton event (Handler h))

-- | Dispatch all the event handler callbacks for a given event type and its
-- corresponding data. Event handlers for this event type will be called in the
-- order they were registered (left to right) with the result of the previous
-- handler fed as the input to the next one.
handle :: Applicative m => Handlers m a -> EventType d -> d -> [Target] -> a -> m a
handle (Handlers allHandlers) event d path a =
  let Handler handler =
        fromMaybe (Handler (const (const (const (pure a)))))
                  (DMap.lookup event allHandlers)
  in handler d path a

-- | A set of handlers for events, possibly empty. Create new 'Handlers' using
-- 'on', and combine 'Handlers' together using their 'Semigroup' instance.
newtype Handlers m a
  = Handlers (DMap EventType (Handler m a))

instance Monad m => Semigroup (Handlers m a) where
  Handlers hs <> Handlers hs' =
    Handlers (DMap.unionWithKey (const (<>)) hs hs')

instance Monad m => Monoid (Handlers m a) where
  mempty = Handlers mempty

-- | A 'Target' is a description of a single element node in the browser. You
-- can query the value of any of an 'attribute', or you can ask for its
-- 'tagName'.
data Target = Target
  { targetTagName :: Text
  , targetAttributes :: HashMap Text Text
  } deriving (Eq, Ord, Show, Generic, JSON.FromJSON)

-- | Get the value, if any, of some named attribute of a 'Target'.
attribute :: Text -> Target -> Maybe Text
attribute name Target{targetAttributes} = HashMap.lookup name targetAttributes

-- | Get the name of the HTML tag for this 'Target'. Note that unlike in the
-- browser itself, Myxine returns tag names in lower case, rather than upper.
tagName :: Target -> Text
tagName Target{targetTagName} = targetTagName

-- | A handler for a single event type, which may consist of one or more event
-- handlers for that particular event.
newtype Handler m a d
  = Handler (d -> [Target] -> a -> m a)

instance Monad m => Semigroup (Handler m a d) where
  Handler h <> Handler g =
    Handler (\d p a -> h d p a >>= g d p)

-- | Given an event name and properties as raw bytestrings, invoke the given
-- callback if the event parses properly, or return 'Nothing'.
withParsedEvent :: ByteString -> ByteString -> (forall d. EventType d -> d -> r) -> Maybe r
withParsedEvent name properties k = do
  Some e <- decodeSomeEventType name
  k e <$> decodeEventProperties e properties
