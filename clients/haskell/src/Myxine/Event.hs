{-# language TemplateHaskell, StrictData, DeriveGeneric, DeriveAnyClass, GADTs,
    KindSignatures, StandaloneDeriving, OverloadedStrings, EmptyCase,
    DuplicateRecordFields #-}

module Myxine.Event where

import Data.FileEmbed
import Myxine.TH

mkEventsAndInterfaces $(embedFile "enabled-events.json")
