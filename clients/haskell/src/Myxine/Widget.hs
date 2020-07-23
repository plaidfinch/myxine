{-| Description : An opinionated widget builder framework for making it quick and
convenient to use the main 'Myxine.Page.Page' abstraction.

This module opinionatedly chooses a specific HTML templating library
('Text.Blaze'), and provides combinators for interleaving structured typed HTML
with event listeners, so you can write your handlers inline with the elements
they pertain to.
-}

module Myxine.Widget
  ( widget, Widget, Listeners, title, html, listen, on, on',
    (@@), (.@@), WidgetM, ListenersM, Html
  ) where

import Prelude hiding (span)
import Text.Blaze.Html5 (Html, (!), dataAttribute, span)
import Text.Blaze.Renderer.Text

import Data.String
import Data.List hiding (span)
import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Lazy as Text

import Myxine.Event
import Myxine.Direct (PageContent, pageBody, pageTitle)
import Myxine.Handlers

data WidgetBuilder model =
  WidgetBuilder
    { widgetLocation :: !(NonEmpty Word)
    , widgetHandlers :: !(Handlers model)
    , widgetMarkup :: !Html
    , widgetPageTitle :: !(Last Text)
    }

-- | The underlying builder monad for the 'Widget' type. This is almost always
-- used with a return type of @()@, hence you will usually see it aliased as
-- 'Widget' @model@.
newtype WidgetM model a =
  WidgetM (State (WidgetBuilder model) a)
  deriving newtype (Functor, Applicative, Monad)

-- | The underlying builder monad for the 'Listeners' type. This is almost
-- always used with a return type of @()@, hence you will usually see it aliased
-- as 'Listeners' @model@.
newtype ListenersM model a =
  ListenersM (ReaderT [Word] (State (Handlers model)) a)
  deriving newtype (Functor, Applicative, Monad)

-- | A 'Widget' is a combination of a view in HTML (described in the markup
-- language of the 'Text.Blaze' library), and a hierarchical set of 'Listeners'
-- which describe how to change the model of the whole page given events that
-- happen to a particular piece of its view.
--
-- This type is meant to be used in @do@-notation, akin to how elements are laid
-- out using successive statements in plain Blaze markup. Create new widgets
-- using 'html', 'listen', 'title', and '@@'.
type Widget model = WidgetM model ()

-- | A set of 'Listeners' is a list of actions, keyed by the DOM event they
-- correspond to, and scoped to only the location in the widget where they are
-- described.
--
-- Attach new listeners to the current location in the DOM using 'listen', and
-- hierarchically attach them to particular sub-elements using '@@'.
-- Individual listeners are created using 'on' or 'on''.
type Listeners model = ListenersM model ()

-- | Wrap an inner widget in some enclosing HTML and/or listeners that pertain
-- to the whole thing. The specified listeners will fire for any event that
-- occurs within this widget, unless it is stopped from propagation within its
-- handler.
(@@) :: (Html -> Html) -> Listeners model -> Widget model -> Widget model
(wrap @@ ListenersM listeners) (WidgetM inner) = WidgetM do
  builder <- get
  let originalLoc = widgetLocation builder
      handlers = execState (runReaderT listeners (NonEmpty.toList originalLoc)) mempty
      builder' = execState inner builder{widgetLocation = 0 <| originalLoc}
  put builder'
    { widgetLocation = let (h :| t) = originalLoc in (h + 1 :| t)
    , widgetMarkup =
      do widgetMarkup builder
         wrapInTarget originalLoc $ wrap (widgetMarkup builder')
    , widgetHandlers = handlers <> widgetHandlers builder' }
  where
    wrapInTarget :: NonEmpty Word -> Html -> Html
    wrapInTarget loc =
      span ! dataAttribute clientDataAttr (showLoc loc)
infixr 5 @@

-- | Create a new atomic widget by attaching a set of listeners to some HTML.
-- This is equivalent to using '@@' with an empty inner widget.
(.@@) :: Html -> Listeners model -> Widget model
markup .@@ handlers = (const markup @@ handlers) (pure ())
infixr 5 .@@

-- | Create a widget with no listeners, that is, embed some arbitrary HTML in
-- the 'Widget' monad.
html :: Html -> Widget model
html markup = markup .@@ pure ()

-- | Set the title for the page. If this function is called multiple times in
-- one update, the most recent call is used.
title :: Text -> Widget model
title t = WidgetM (modify \wb -> wb { widgetPageTitle = Last (Just t) })

-- | Add some listeners to the /current/ widget context. Note that writing two
-- consecutive lines using 'html' and 'listen' is /not/ the same as using the
-- '.@@' combinator: the former attaches the listeners to the /enclosing/
-- /context/, and the latter attaches them to the /specified HTML/.
listen :: Listeners model -> Widget model
listen (ListenersM listeners) = WidgetM do
  builder <- get
  let originalLoc = widgetLocation builder
      handlers = execState (runReaderT listeners (NonEmpty.tail originalLoc)) mempty
  modify \builder' -> builder { widgetHandlers = handlers <> widgetHandlers builder' }

-- | Listen to a particular event and react to it by modifying the model for the
-- page, specifying whether or not to propagate the event outwards to other
-- enclosing contexts.
on' :: EventType props -> (props -> StateT model IO Propagation) -> Listeners model
on' event action = ListenersM do
  loc <- ask
  let widgetSelector =
        case loc of
          [] -> mempty
          h : t -> ("data-" <> clientDataAttr)
            `attrIs` (Text.toStrict . Text.pack . showLoc $ h :| t)
  modify . mappend $
    onEvent event widgetSelector $
    \props model -> runStateT (action props) model

-- | Listen to a particular event and react to it by modifying the model for the
-- page. This event will bubble out to listeners in enclosing contexts; to
-- prevent this, use 'on''.
on :: EventType props -> (props -> StateT model IO ()) -> Listeners model
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

-- | Evaluate a widget to produce a pair of 'PageContent' and 'Handlers'. This
-- is the bridge between the 'runPage' abstraction and the 'Widget' abstraction:
-- use this to run a widget in a 'Page'.
widget :: Widget model -> (PageContent, Handlers model)
widget (WidgetM action) =
  let WidgetBuilder{widgetHandlers, widgetMarkup, widgetPageTitle = pageContentTitle} =
        execState action initialBuilder
      pageContentBody = renderMarkup widgetMarkup
  in (pageBody (Text.toStrict pageContentBody)
      <> foldMap pageTitle pageContentTitle,
      widgetHandlers)
  where
    initialBuilder = WidgetBuilder
      { widgetLocation = (0 :| [])
      , widgetHandlers = mempty
      , widgetMarkup = pure ()
      , widgetPageTitle = Last Nothing }

instance Semigroup a => Semigroup (WidgetM model a) where
  m <> n = (<>) <$> m <*> n

instance Semigroup a => Semigroup (ListenersM model a) where
  m <> n = (<>) <$> m <*> n

instance Monoid a => Monoid (WidgetM model a) where mempty = pure mempty
instance Monoid a => Monoid (ListenersM model a) where mempty = pure mempty
