{-# language StrictData #-}
{-# options_ghc -Wno-name-shadowing -Wno-unused-matches #-}
{-# options_haddock not-home #-}

module Myxine.Internal.Event where

import Data.FileEmbed
import Myxine.Internal.TH

mkEventsAndInterfaces $(embedFile "enabled-events.json")
