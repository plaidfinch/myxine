{-# language GADTs, TypeFamilies, DataKinds, DuplicateRecordFields, RankNTypes,
    TypeApplications, StrictData, ScopedTypeVariables, BlockArguments,
    OverloadedLabels, PolyKinds, UndecidableInstances, UndecidableSuperClasses,
    MultiParamTypeClasses, FlexibleInstances, TypeOperators, FlexibleContexts,
    AllowAmbiguousTypes #-}

module Myxine
  ( Handlers(..)
  , Properties
  , onEvent
  , dispatchEvent
  ) where

import Data.Text (Text)
import Data.Kind
import GHC.TypeLits
import GHC.OverloadedLabels
import GHC.Records
import Data.Functor.Identity
import Data.Functor.Const

onEvent ::
  forall e m. (IsEvent e, Applicative m) =>
  (Properties (Interface e) -> m ()) ->
  Handlers m
onEvent h =
  runIdentity $ (handler @e) (\_ -> Identity h) mempty

dispatchEvent ::
  forall e m. (IsEvent e, Applicative m) =>
  Properties (Interface e) ->
  Handlers m ->
  m ()
dispatchEvent properties handlers =
  (getConst $ (handler @e) Const handlers) properties

class IsInterface (Interface e) => IsEvent (e :: Symbol) where
  type Interface e :: Symbol
  -- This is a lens, but for what I need I don't need Control.Lens
  handler :: forall f m.
    Functor f =>
    ((Properties (Interface e) -> m ()) ->
     f (Properties (Interface e) -> m ())) ->
    (Handlers m -> f (Handlers m))

class IsInterface (i :: Symbol) where
  data Properties i :: Type

-- Everything below here needs to get generated

data Handlers m = Handlers
  { onClick :: Properties "MouseEvent" -> m ()
  , onInput :: Properties "InputEvent" -> m ()
  {- ... for every event ... -}
  }

instance Applicative m => Semigroup (Handlers m) where
  Handlers a b {- ... for every event ... -}
    <> Handlers a' b' {- ... for every event ... -}
    = Handlers (a *> a') (b *> b') {- ... for every event ... -}

instance Applicative m => Monoid (Handlers m) where
  mempty =
    Handlers u u {- ... for every event ... -}
    where u = const (pure ())

instance IsInterface "MouseEvent" where
  data Properties "MouseEvent" = MouseEventProperties
    { _clientX :: Int
    , _clientY :: Int
    {- ... for every (transitive) property of this interface ... -}
    }

instance IsInterface "InputEvent" where
  data Properties "InputEvent" = InputEventProperties
    { _data :: Text
    , _inputType :: Text
    {- ... for every (transitive) property of this interface ... -}
    }

{- ... for every interface ... -}

instance IsEvent "click" where
  type Interface "click" = "MouseEvent"
  handler f handlers = fmap (\h -> handlers { onClick = h }) (f (onClick handlers))

instance IsEvent "input" where
  type Interface "input" = "InputEvent"
  handler f handlers = fmap (\h -> handlers { onInput = h }) (f (onInput handlers))

{- ... for every event ... -}

instance (HasField "_clientX" (Properties i) t) => IsLabel "clientX" (Properties i -> t) where
  fromLabel = getField @"_clientX"

instance (HasField "_clientY" (Properties i) t) => IsLabel "clientY" (Properties i -> t) where
  fromLabel = getField @"_clientY"

instance (HasField "_data" (Properties i) t) => IsLabel "data" (Properties i -> t) where
  fromLabel = getField @"_data"

instance (HasField "_inputType" (Properties i) t) => IsLabel "inputType" (Properties i -> t) where
  fromLabel = getField @"_inputType"

{- ... for every distinct property ... -}
