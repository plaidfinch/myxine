{-# language GADTs, DataKinds, DuplicateRecordFields, RankNTypes, StrictData,
    ScopedTypeVariables, BlockArguments, KindSignatures, TemplateHaskell,
    OverloadedStrings, DerivingStrategies, DerivingVia, StandaloneDeriving,
    DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Myxine
  (
  -- * Types of Events
  EventType(..)

  -- * Targets for Events
  , module Myxine.Target

  -- * Handling Events
  , module Myxine.Handlers

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

import Myxine.Target
import Myxine.Handlers
import Myxine.Event
import qualified Myxine.Event as Event
  hiding (decodeSomeEventType, decodeEventProperties, EventType(..))
