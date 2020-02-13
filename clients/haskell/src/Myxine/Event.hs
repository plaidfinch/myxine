{-# language TemplateHaskell, StrictData, DeriveGeneric, DeriveAnyClass, GADTs,
    KindSignatures, StandaloneDeriving, OverloadedStrings #-}

module Myxine.Event where

-- TODO: These imports will go away once the TH is finished
import Data.GADT.Compare
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson as JSON
import Data.Dependent.Map (Some(..))

import Myxine.TH

-- TH deriving the things

mkInterface "MouseEvent"
  [("clientX", "i64"), ("clientY", "i64")]

mkInterface "InputEvent"
  [("data", "i64"), ("inputType", "String")]

mkEventEnum "Event" [(["click"], ''MouseEvent), (["input"], ''InputEvent)]

mkEnumGEqInstance ''Event

deriving instance Eq (Event d)
deriving instance Ord (Event d)
deriving instance Show (Event d)

-- TODO: Everything below here isn't yet TH-ified

instance GCompare Event where
  gcompare e1 e2 =
    case e1 of
      Click -> case e2 of
        Click -> GEQ
        Input -> GLT
        {- ... for every event ... -}
      Input -> case e2 of
        Click -> GGT
        Input -> GEQ
        {- ... for every event ... -}
      {- ... for every event ... -}

allEvents :: HashMap ByteString (Some Event)
allEvents = HashMap.fromList
  [ ("click", Some Click)
  , ("input", Some Input)
  {- ... for every event ... -}
  ]

decodeProperties :: Event d -> ByteString -> Maybe d
decodeProperties event =
  case event of
    Click -> JSON.decode
    Input -> JSON.decode
    {- ... for every event ... -}
