{-|
    Description : High-level "model-view-controller" interface to the Myxine server

    This library implements well-typed bindings to the
    [Myxine](https://github.com/GaloisInc/myxine) server for creating local
    interactive GUIs in the web browser. For more details on Myxine-the-program, see
    the package description of this library, or [its own
    homepage](https://github.com/GaloisInc/myxine).

    This module defines a higher level "model-view-controller" style interface in
    which abstracts over the direct calls to the Myxine server to allow a more
    declarative style of programming.

    For a one-to-one set of bindings directly to the corresponding calls to the
    Myxine API see the module "Myxine.Direct". This is straightforward for small
    examples and tests, but can become cumbersome for building full interactive
    applications.

    __Required extensions:__ This library relies on the @OverloadedRecordFields@
    language extension, since a variety of browser event interfaces share field
    names/types. Without enabling it, you'll see many bewildering errors about
    ambiguous names. You may also find useful for concision the extensions
    @NamedFieldPuns@ and @RecordWildCards@.
-}
module Myxine
  ( module Myxine.Page

  -- * #Types# Types and Properties of Events

-- | These types are automatically generated from Myxine's master specification
-- of supported events and interfaces, so they will always match those
-- supported by the version of Myxine corresponding to the version of this
-- library. However, Template Haskell does not allow programmatic generation
-- of Haddock documentation, so we can't put proper inline documentation
-- below.
--
-- To aid in your reference, note that the name of each type below exactly
-- matches the browser's name for events of that interface, and the names of
-- each interface's properties exactly match the browser's names for them,
-- except in the cases where those names are reserved keywords in Haskell. In
-- those cases, we prepend the name of the interface (for instance, we use the
-- property name @inputData@ instead of @data@).
--
-- For more details on the meaning of each type below and its fields, refer to
-- Myxine's documentation and/or the [MDN web API documentation for events and
-- their interfaces](https://developer.mozilla.org/docs/Web/Events).
  , module Event
  , module Data.Aeson
  , module Control.Monad.IO.Class
  ) where

import Myxine.Event
import Myxine.Direct
import Myxine.Page
import qualified Myxine.Event as Event
  hiding (decodeSomeEventType, eventPropertiesDict, encodeEventType)

import Data.Aeson (FromJSON)
import Control.Monad.IO.Class (MonadIO(..))
