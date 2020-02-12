{-# language GADTs, TypeFamilies, DataKinds, DuplicateRecordFields, RankNTypes,
    TypeApplications, StrictData, ScopedTypeVariables, BlockArguments,
    OverloadedLabels, PolyKinds, UndecidableInstances, UndecidableSuperClasses,
    MultiParamTypeClasses, FlexibleInstances, TypeOperators, FlexibleContexts,
    AllowAmbiguousTypes, ConstraintKinds, TemplateHaskell, OverloadedStrings #-}

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
import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON)
import Data.Aeson.TH
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Proxy

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

dispatchSomeEvent :: Applicative m => SomeEvent -> Handlers m -> m ()
dispatchSomeEvent
  (SomeEvent (Proxy :: Proxy e) (properties :: Properties (Interface e))) handlers =
  dispatchEvent @e properties handlers

data SomeEvent where
  SomeEvent :: IsEvent e => Proxy e -> Properties (Interface e) -> SomeEvent

parseEvent :: ByteString -> ByteString -> Maybe SomeEvent
parseEvent name properties = do
  parser <- HashMap.lookup name parsers
  parser properties
  where
    parsers :: HashMap ByteString (ByteString -> Maybe SomeEvent)
    parsers = HashMap.fromList eventParsers

parseEventNamed :: forall e. IsEvent e => ByteString -> Maybe SomeEvent
parseEventNamed = fmap (SomeEvent (Proxy @e)) . JSON.decode

class IsInterface (Interface e) => IsEvent (e :: Symbol) where
  type Interface e :: Symbol
  -- This is a lens, but for what I need I don't need Control.Lens
  handler :: forall f m.
    Functor f =>
    ((Properties (Interface e) -> m ()) ->
     f (Properties (Interface e) -> m ())) ->
    (Handlers m -> f (Handlers m))

class (FromJSON (Properties i)) => IsInterface (i :: Symbol) where
  data Properties i :: Type

-- Pretty error messages

type family If (b :: Bool) (t :: k) (e :: k) :: k where
  If 'True t  _ = t
  If 'False _ f = f

type family In (e :: k) (es :: [k]) :: Bool where
  In _ '[]       = 'False
  In e (e ': _)  = 'True
  In e (_ ': es) = In e es

type family ShowListOfTypes (ts :: [k]) :: ErrorMessage where
  ShowListOfTypes '[] = 'Text ""
  ShowListOfTypes '[t] = 'Text " ⚬ " ':<>: 'ShowType t
  ShowListOfTypes (t ': ts) = 'Text " ⚬ " ':<>: 'ShowType t ':$$: ShowListOfTypes ts

type family JoinErrMessage (between :: Symbol) (errs :: [ErrorMessage]) where
  JoinErrMessage _ '[] = 'Text ""
  JoinErrMessage _ '[e] = e
  JoinErrMessage between (e ': es) =
    e ':<>: 'Text between ':<>: JoinErrMessage between es

type family AssertEqual (err :: ErrorMessage) (t :: k) (s :: l) :: Constraint where
  AssertEqual _ t t = ()
  AssertEqual err _ _ = TypeError err

type PropertyOfType f i t s =
  AssertEqual ('Text "Couldn't match expected type ‘" ':<>: 'ShowType t ':<>:
               'Text "’ with actual type ‘" ':<>: 'ShowType s ':<>:
               'Text "’" ':$$: 'Text "in property " ':<>: 'ShowType f ':<>:
               'Text " of event interface " ':<>: 'ShowType i) t s

instance {-# OVERLAPPABLE #-}
  (TypeError (If (In i AllInterfaces)
              ('Text "No property named " ':<>:
               'ShowType f ':<>:
               'Text " in the event interface " ':<>:
               'ShowType i ':$$:
                  ('Text "Did you mean any of: " ':$$:
                   (ShowListOfTypes (PropertiesOf i))))
               ('Text "No event interface named " ':<>: 'ShowType i ':$$:
                'Text "Did you mean any of: " ':$$:
                 ShowListOfTypes AllInterfaces))) =>
  IsLabel f (Properties i -> t) where
  fromLabel = error "unreachable"

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

instance (PropertyOfType "clientX" "MouseEvent" Int t, t ~ Int) => IsLabel "clientX" (Properties "MouseEvent" -> t) where
  fromLabel = getField @"_clientX"

instance (PropertyOfType "clientY" "MouseEvent" Int t, t ~ Int) => IsLabel "clientY" (Properties "MouseEvent" -> t) where
  fromLabel = getField @"_clientY"

instance (PropertyOfType "data" "InputEvent" Text t, t ~ Text) => IsLabel "data" (Properties "InputEvent" -> t) where
  fromLabel = getField @"_data"

instance (PropertyOfType "inputType" "InputEvent" Text t, t ~ Text) => IsLabel "inputType" (Properties "InputEvent" -> t) where
  fromLabel = getField @"_inputType"

{- ... for every interface, for every property ... -}

type AllInterfaces =
  '["MouseEvent", "InputEvent" {- ... for every interface, sorted ... -}]

type family PropertiesOf (i :: Symbol) :: [Symbol] where
  PropertiesOf "MouseEvent" = ["clientX", "clientY" {- ... for every property, sorted ... -} ]
  PropertiesOf "InputEvent" = ["data", "inputType" {- ... for every property, sorted ... -} ]
  {- ... for every interface ... -}

eventParsers :: [(ByteString, (ByteString -> Maybe SomeEvent))]
eventParsers =
  [ ("click", parseEventNamed @"click")
  , ("input", parseEventNamed @"input")
  ]

$(deriveFromJSON defaultOptions{fieldLabelModifier = tail} 'MouseEventProperties)
$(deriveFromJSON defaultOptions{fieldLabelModifier = tail} 'InputEventProperties)
{- ... for every interface ... -}
