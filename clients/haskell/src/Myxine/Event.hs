{- | Description : Types and properties of browser events

These types are automatically generated from Myxine's master specification
of supported events and interfaces, so they will always match those
supported by the version of Myxine corresponding to the version of this
library. However, Template Haskell does not allow programmatic generation
of Haddock documentation, so we can't put proper inline documentation
below.

To aid in your reference, note that the name of each type below exactly
matches the browser's name for events of that interface, and the names of
each interface's properties exactly match the browser's names for them,
except in the cases where those names are reserved keywords in Haskell. In
those cases, we prepend the name of the interface (for instance, we use the
property name @inputData@ instead of @data@).

For more details on the meaning of each type below and its fields, refer to
Myxine's documentation and/or the [MDN web API documentation for events and
their interfaces](https://developer.mozilla.org/docs/Web/Events).
-}
{-# language StrictData #-}
{-# options_ghc -Wno-name-shadowing -Wno-unused-matches #-}

module Myxine.Event (module Myxine.Internal.Event) where

import Myxine.Internal.Event hiding
  (decodeSomeEventType, eventPropertiesDict, encodeEventType)
