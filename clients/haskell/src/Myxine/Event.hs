{-# language TemplateHaskell, StrictData, DeriveGeneric, DeriveAnyClass, GADTs,
    KindSignatures, StandaloneDeriving, OverloadedStrings, EmptyCase,
    DuplicateRecordFields #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Myxine.Event where

import Myxine.TH

mkEventsAndInterfaces "enabled-events.json"
