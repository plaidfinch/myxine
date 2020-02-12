{-# language GADTs, DataKinds, DuplicateRecordFields, RankNTypes, StrictData,
    ScopedTypeVariables, BlockArguments, KindSignatures, TemplateHaskell,
    OverloadedStrings, DerivingStrategies, DerivingVia, StandaloneDeriving,
    DeriveGeneric, DeriveAnyClass #-}

module Myxine
  ( Handlers
  , on
  , handler
  , Target(..)
  ) where

import Data.Text (Text)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Typeable
import Data.GADT.Compare
import Data.Dependent.Map (DMap, Some(..))
import qualified Data.Dependent.Map as DMap
import Data.Monoid

import Myxine.TH

-- TH deriving the things

mkInterface "MouseEvent"
  [("clientX", "i64"), ("clientY", "i64")]

mkInterface "InputEvent"
  [("data", "i64"), ("inputType", "String")]

mkEventEnum "Event" [(["click"], ''MouseEvent), (["input"], ''InputEvent)]

mkEnumGEqInstance ''Event

-- The actual code

on :: Event d -> (d -> [Target] -> a -> m b) -> Handlers m a b
on event h = Handlers (DMap.singleton event (Handler h))

handler :: Event d -> Handlers m a b -> Maybe (d -> [Target] -> a -> m b)
handler event (Handlers handlers) = do
  Handler h <- DMap.lookup event handlers
  return h

newtype Handlers m a b
  = Handlers (DMap Event (Handler m a b))

data Target = Target
  { tagName :: Text
  , attributes :: HashMap Text Text
  } deriving (Eq, Ord, Show)

newtype Handler m a b d
  = Handler (d -> [Target] -> a -> m b)

withEvent :: ByteString -> ByteString -> (forall d. Event d -> d -> r) -> Maybe r
withEvent name properties k = do
  Some e <- decodeSomeEvent name
  k e <$> decodeProperties e properties
  where
    decodeSomeEvent :: ByteString -> Maybe (Some Event)
    decodeSomeEvent = flip HashMap.lookup allEvents

deriving via (Dual (DMap Event (Handler m a b))) instance Semigroup (Handlers m a b)
deriving via (Dual (DMap Event (Handler m a b))) instance Monoid (Handlers m a b)
deriving instance Eq (Event d)
deriving instance Ord (Event d)
deriving instance Show (Event d)

-- Everything below here isn't yet TH-ified

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
