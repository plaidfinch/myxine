{-# language TemplateHaskell, StrictData, DeriveGeneric, DeriveAnyClass, GADTs,
    KindSignatures, StandaloneDeriving, OverloadedStrings, EmptyCase,
    DuplicateRecordFields, DerivingStrategies #-}
{-# options_ghc -Wno-name-shadowing -Wno-unused-matches #-}

module Myxine.Event where

import Data.FileEmbed
import Myxine.TH

mkEventsAndInterfaces $(embedFile "enabled-events.json")

-- TODO: Use an associated data family for EventType, to allow forward compat
