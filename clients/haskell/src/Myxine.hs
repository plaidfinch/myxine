{-# language GADTs, DataKinds, DuplicateRecordFields, RankNTypes, StrictData,
    ScopedTypeVariables, BlockArguments, KindSignatures, TemplateHaskell,
    OverloadedStrings, DerivingStrategies, DerivingVia, StandaloneDeriving,
    DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving #-}

module Myxine
  ( Handlers
  , EventType(..)
  , on
  , handle
  , withEvent
  , decodeSomeEventType
  , decodeEventProperties
  , Target(..)
  , module Event
  ) where

import Data.Text (Text)
import qualified Data.Aeson as JSON
import Data.HashMap.Lazy (HashMap)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (ByteString)
import Data.Dependent.Map (DMap, Some(..))
import qualified Data.Dependent.Map as DMap
import Data.Traversable
import Data.Maybe
import GHC.Generics

import Myxine.Event
import qualified Myxine.Event as Event
  hiding (decodeSomeEventType, decodeEventProperties, EventType(..))

-- The actual code

on :: EventType d -> (d -> [Target] -> a -> m b) -> Handlers m a b
on event h = Handlers (DMap.singleton event (Handler [h]))

handle :: Applicative m => EventType d -> Handlers m a b -> d -> [Target] -> a -> m [b]
handle event (Handlers allHandlers) d path a =
  let Handler handlers =
        fromMaybe (Handler []) (DMap.lookup event allHandlers)
  in for handlers \handler ->
    handler d path a

newtype Handlers m a b
  = Handlers (DMap EventType (Handler m a b))

instance Semigroup (Handlers m a b) where
  Handlers hs <> Handlers hs' =
    Handlers (DMap.unionWithKey (const (<>)) hs hs')

instance Monoid (Handlers m a b) where
  mempty = Handlers mempty

data Target = Target
  { tagName :: Text
  , attributes :: HashMap Text Text
  } deriving (Eq, Ord, Show, Generic, JSON.FromJSON)

newtype Handler m a b d
  = Handler [d -> [Target] -> a -> m b]
  deriving newtype (Semigroup, Monoid)

withEvent :: ByteString -> ByteString -> (forall d. EventType d -> d -> r) -> Maybe r
withEvent name properties k = do
  Some e <- decodeSomeEventType name
  k e <$> decodeEventProperties e properties
