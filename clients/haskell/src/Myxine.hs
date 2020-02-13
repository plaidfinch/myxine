{-# language GADTs, DataKinds, DuplicateRecordFields, RankNTypes, StrictData,
    ScopedTypeVariables, BlockArguments, KindSignatures, TemplateHaskell,
    OverloadedStrings, DerivingStrategies, DerivingVia, StandaloneDeriving,
    DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving #-}

module Myxine
  ( Handlers
  , Event(..)
  , on
  , handle
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
import Data.Traversable
import Data.Maybe

import Myxine.Event

-- The actual code

on :: Event d -> (d -> [Target] -> a -> m b) -> Handlers m a b
on event h = Handlers (DMap.singleton event (Handler [h]))

handle :: Applicative m => Event d -> Handlers m a b -> d -> [Target] -> a -> m [b]
handle event (Handlers allHandlers) d path a =
  let Handler handlers =
        fromMaybe (Handler []) (DMap.lookup event allHandlers)
  in for handlers \handler ->
    handler d path a

newtype Handlers m a b
  = Handlers (DMap Event (Handler m a b))

instance Semigroup (Handlers m a b) where
  Handlers hs <> Handlers hs' =
    Handlers (DMap.unionWithKey (const (<>)) hs hs')

instance Monoid (Handlers m a b) where
  mempty = Handlers mempty

data Target = Target
  { tagName :: Text
  , attributes :: HashMap Text Text
  } deriving (Eq, Ord, Show)

newtype Handler m a b d
  = Handler [d -> [Target] -> a -> m b]
  deriving newtype (Semigroup, Monoid)

withEvent :: ByteString -> ByteString -> (forall d. Event d -> d -> r) -> Maybe r
withEvent name properties k = do
  Some e <- decodeSomeEvent name
  k e <$> decodeProperties e properties
  where
    decodeSomeEvent :: ByteString -> Maybe (Some Event)
    decodeSomeEvent = flip HashMap.lookup allEvents
