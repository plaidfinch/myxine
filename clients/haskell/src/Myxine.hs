{-|
    Description : High-level "model-view-controller" interface to the Myxine server

    This library implements well-typed bindings to the
    [Myxine](https://github.com/GaloisInc/myxine) server for creating local
    interactive GUIs in the web browser. For more details on Myxine-the-program, see
    the package description of this library, or [its own
    homepage](https://github.com/GaloisInc/myxine).

    This module defines a higher level interface which abstracts over the direct
    calls to the Myxine server to allow a more declarative style of programming.

    For a one-to-one set of bindings directly to the corresponding calls to the
    Myxine API see the module "Myxine.Direct". This is straightforward for small
    examples and tests, but can become cumbersome for building full interactive
    applications.
-}
module Myxine
  ( -- ** Required Extensions
{-| This library relies on the __@OverloadedRecordFields@__ language extension,
since a variety of browser event interfaces share field names/types. Without
enabling it, you'll see many bewildering errors about ambiguous names. You may
also find useful for concision the extensions __@NamedFieldPuns@__ and
__@RecordWildCards@__.
-}
    module Myxine.Page
  , module Myxine.Event
  ) where

import Myxine.Event
import Myxine.Direct
import Myxine.Page
