{-# language TemplateHaskell, StrictData, DeriveGeneric, DeriveAnyClass, GADTs,
    KindSignatures, StandaloneDeriving, OverloadedStrings #-}

module Myxine.Event where

-- TODO: These imports will go away once the TH is finished
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Aeson as JSON
import Data.Dependent.Map (Some(..))
import Language.Haskell.TH (mkName)

import Myxine.TH

-- TH deriving the things

mkInterface "MouseEvent"
  [("clientX", "i64"), ("clientY", "i64")]

mkInterface "InputEvent"
  [("data", "i64"), ("inputType", "String")]

mkEventEnum [(["click"], ''MouseEvent), (["input"], ''InputEvent)]

mkEnumGEqInstance eventTypeName
mkEnumGCompareInstance eventTypeName

mkDecodeProperties (mkName "decodeProperties")

deriving instance Eq (Event d)
deriving instance Ord (Event d)
deriving instance Show (Event d)

-- TODO: Everything below here isn't yet TH-ified

allEvents :: HashMap ByteString (Some Event)
allEvents = HashMap.fromList
  [ ("click", Some Click)
  , ("input", Some Input)
  {- ... for every event ... -}
  ]
