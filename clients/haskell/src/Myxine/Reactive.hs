{-|
Description : An opinionated widget builder framework for 'Myxine.Page.Page'

This module chooses a specific HTML templating library ('Text.Blaze'), and
provides combinators for interleaving structured typed HTML with event
listeners, so you can write your handlers inline with the elements to which they
pertain.
-}

module Myxine.Reactive
  ( reactive, title, html, on, on', (@@), Reactive, ReactiveM, Html
  ) where

import Prelude hiding (span)
import Text.Blaze.Html5 (Html, ToMarkup(..), string, (!), dataAttribute)
import Text.Blaze.Renderer.Text
import Text.Blaze.Internal (Attributable)

import Data.String
import Data.List hiding (span)
import Control.Monad.State
import Data.Monoid
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Lazy as Text

import Myxine.Event
import qualified Myxine.Direct as Direct (PageContent, pageBody, pageTitle)
import Myxine.Handlers

data ReactiveBuilder model =
  ReactiveBuilder
    { location  :: !(NonEmpty Word)
    , handlers  :: !(Handlers model)
    , markup    :: !Html
    , pageTitle :: !(Last Text)
    }

-- | The underlying builder monad for the 'Reactive' type. This is almost always
-- used with a return type of @()@, hence you will usually see it aliased as
-- 'Reactive' @model@.
newtype ReactiveM model a =
  ReactiveM (State (ReactiveBuilder model) a)
  deriving newtype (Functor, Applicative, Monad)

-- | A 'Reactive' is a combination of a view in HTML (described in the markup
-- language of the 'Text.Blaze' library), and a hierarchical set of 'Listeners'
-- which describe how to change the model of the whole page given events that
-- happen to a particular piece of its view.
--
-- This type is meant to be used in @do@-notation, akin to how elements are laid
-- out using successive statements in plain Blaze markup. Create new reactive
-- components using 'html', 'listen', 'title', and '@@'.
type Reactive model = ReactiveM model ()

-- | Wrap an inner reactive component in some enclosing HTML. The specified
-- listeners will fire for any event that occurs within this component, unless
-- it is stopped from propagation within its handler.
(@@) :: (Html -> Html) -> ReactiveM model a -> ReactiveM model a
wrap @@ ReactiveM inner = ReactiveM do
  builder <- get
  let originalLoc = location builder
      (result, builder') =
        runState inner builder
          { location = 0 <| originalLoc
          , markup = mempty }
  put builder'
    { location = let (h :| t) = originalLoc in (h + 1 :| t)
    , markup =
      do markup builder
         wrapInTarget originalLoc $ wrap (markup builder') }
  pure result
  where
    wrapInTarget :: NonEmpty Word -> Html -> Html
    wrapInTarget loc =
      (! dataAttribute clientDataAttr (showLoc loc))
infixr 5 @@

-- | Create a component from some arbitrary HTML with no reactive behavior.
html :: ToMarkup h => h -> Reactive model
html markup = const (toMarkup markup) @@ pure ()

-- | Set the title for the page. If this function is called multiple times in
-- one update, the most recent call is used.
title :: Text -> Reactive model
title t = ReactiveM (modify \wb -> wb { pageTitle = Last (Just t) })

-- | Listen to a particular event and react to it by modifying the model for the
-- page, specifying whether or not to propagate the event outwards to other
-- enclosing contexts.
on' :: EventType props -> (props -> StateT model IO Propagation) -> Reactive model
on' event action = ReactiveM do
  loc <- gets (NonEmpty.tail . location)
  let selector =
        case loc of
          [] -> mempty
          h : t -> ("data-" <> clientDataAttr)
            `attrIs` (Text.toStrict . Text.pack . showLoc $ h :| t)
  modify \builder ->
    builder { handlers = mappend (handlers builder) $
              onEvent event selector $
              \props model -> runStateT (action props) model }

-- | Listen to a particular event and react to it by modifying the model for the
-- page. This event will bubble out to listeners in enclosing contexts; to
-- prevent this, use 'on''.
on :: EventType props -> (props -> StateT model IO ()) -> Reactive model
on event action = on' event (\props -> action props >> pure Bubble)

-- | The in-browser name for the data attribute holding our tracking id. This is
-- not the same as the @id@ attribute, because this means the user is free to
-- use the _real_ @id@ attribute as they please.
clientDataAttr :: IsString a => a
clientDataAttr = "myxine-client-widget-id"

-- | Helper function to show a location in the page: add hyphens between every
-- number.
showLoc :: IsString a => NonEmpty Word -> a
showLoc = fromString . intercalate "-" . map show . NonEmpty.toList

-- | Evaluate a reactive component to produce a pair of 'PageContent' and
-- 'Handlers'. This is the bridge between the 'runPage' abstraction and the
-- 'Reactive' abstraction: use this to run a reactive component in a 'Page'.
reactive :: Reactive model -> (Direct.PageContent, Handlers model)
reactive (ReactiveM action) =
  let ReactiveBuilder{handlers, markup, pageTitle = pageContentTitle} =
        execState action initialBuilder
      pageContentBody = renderMarkup markup
  in (Direct.pageBody (Text.toStrict pageContentBody)
      <> foldMap Direct.pageTitle pageContentTitle,
      handlers)
  where
    initialBuilder = ReactiveBuilder
      { location = (0 :| [])
      , handlers = mempty
      , markup = pure ()
      , pageTitle = Last Nothing }

instance Semigroup a => Semigroup (ReactiveM model a) where
  m <> n = (<>) <$> m <*> n

instance Monoid a => Monoid (ReactiveM model a) where mempty = pure mempty

instance Attributable (ReactiveM model a) where
  w ! a = (! a) @@ w

instance Attributable (ReactiveM model a -> ReactiveM model a) where
  f ! a = (! a) . f

instance (a ~ ()) => IsString (ReactiveM model a) where
  fromString = html . string
