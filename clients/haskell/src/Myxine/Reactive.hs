{-|
Description : An opinionated widget builder for 'Myxine.Page.Page'

While in theory, it is equally possible to write the content of your page and
its event listeners separately, it is often most intelligible to connect them
together, as many pages have event listeners that are scoped to a particular
section of their HTML. This module provides a tiny DSL for describing pages in
these terms, with event handlers inline with the elements to which they pertain.

In our nomenclature, a reactive component is an interleaving of a description of
the literal HTML structure of the page, and the event handlers which are scoped
to various sections of that page. They are built in the 'ReactiveM' monad, and
once built, are interpreted using:

> 'reactive' :: 'Reactive' model -> ('Direct.PageContent', 'Handlers' model)

Note that this output type is precisely the right type to hand to 'runPage' in
the return type of its last function argument:

> 'runPage' ::
>   'PageLocation' ->
>   model ->
>   (model -> 'Direct.PageContent', 'Handlers' model) ->
>   IO ('Page' model)

This means that running a page using this technique looks like:

> runPage location initialModel (reactive component)

where @component :: Reactive model@.

__Helpful companions:__

This module is meant to be imported alongside the 'Text.Blaze.Html5' and
'Text.Blaze.Html5.Attributes' modules from the @blaze-html@ package, which
provide the HTML combinators required for building markup.

While it does not hard-code the use of lenses, in the body of handlers given to
'on', it is often useful to manipulate the model state using lens combinators,
in particular the stateful 'Control.Lens.Setter..=' and friends.
-}

module Myxine.Reactive
  ( reactive, title, html, on, on', target, (@@), Reactive, ReactiveM
  , Html, ToMarkup(..)
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

-- | The builder state for a reactive component.
data ReactiveBuilder model =
  ReactiveBuilder
    { location  :: !(NonEmpty Word)
    -- ^ The current location in the tree of listener scopes we've created.
    -- Calls to 'on' refer to the enclosing scope (that is, the tail of this
    -- list).
    , handlers  :: !(Handlers model)
    -- ^ The accumulated handlers for events built up so far.
    , markup    :: !Html
    -- ^ The accumulated markup for the page built up so far.
    , pageTitle :: !(Last Text)
    -- ^ The most-recently set title for the page (via 'title').
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

-- | Wrap an inner reactive component in some enclosing HTML. Any listeners
-- created via 'on' in the wrapped component will be scoped to only events that
-- occur within this chunk of HTML.
(@@) :: (Html -> Html) -> ReactiveM model a -> ReactiveM model a
wrap @@ ReactiveM inner = ReactiveM do
  builder <- get
  let originalLoc = location builder
      (result, builder') =
        runState inner builder
          { location = 0 <| originalLoc  -- descend a level in location cursor
          , markup = mempty
          -- handlers & pageTitle are threaded through and preserved
          }
  put builder'
    { location = let (h :| t) = originalLoc in (h + 1 :| t)  -- move sideways
    , markup =
      do markup builder
         wrapInTarget originalLoc $ wrap (markup builder')
    -- handlers & pageTitle are threaded through and preserved
    }
  pure result
  where
    wrapInTarget :: NonEmpty Word -> Html -> Html
    wrapInTarget loc =
      (! dataAttribute clientDataAttr (showLoc loc))
infixr 5 @@

-- | Create a scope for event listeners without wrapping any enclosing HTML. Any
-- listeners created via 'on' will apply only to HTML that is written inside
-- this block, rather than to the enclosing broader scope.
--
-- > target === (id @@)
target :: ReactiveM model a -> ReactiveM model a
target = (id @@)

-- | Write an atomic piece of HTML (or anything that can be converted to it) to
-- the page in this location. Event listeners for its enclosing scope can be
-- added by sequential use of 'on'. If you need sub-pieces of this HTML to have
-- their own scoped event listeners, use '@@' to build a composite component.
--
-- > html h === const (toMarkup h) @@ pure ()
html :: ToMarkup h => h -> Reactive model
html h = const (toMarkup h) @@ pure ()

-- | Set the title for the page. If this function is called multiple times in
-- one update, the most recent call is used.
title :: Text -> Reactive model
title t = ReactiveM (modify \wb -> wb { pageTitle = Last (Just t) })

-- | Listen to a particular event and react to it by modifying the model for the
-- page, specifying whether or not to propagate the event outwards to other
-- enclosing contexts. The event target is scoped to the enclosing '@@', or the
-- whole page if at the top level.
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
-- prevent this, use 'on''. The event target is scoped to the enclosing '@@', or
-- the whole page if it is at the top level.
on :: EventType props -> (props -> StateT model IO ()) -> Reactive model
on event action = on' event (\props -> action props >> pure Bubble)

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

-- | The in-browser name for the data attribute holding our tracking id. This is
-- not the same as the @id@ attribute, because this means the user is free to
-- use the _real_ @id@ attribute as they please.
clientDataAttr :: IsString a => a
clientDataAttr = "myxine-client-widget-id"

-- | Helper function to show a location in the page: add hyphens between every
-- number.
showLoc :: IsString a => NonEmpty Word -> a
showLoc = fromString . intercalate "-" . map show . NonEmpty.toList
