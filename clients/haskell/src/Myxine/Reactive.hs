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

@
'reactive' :: 'Reactive' model -> ('Direct.PageContent', 'Handlers' model)
@

Note that this output type is precisely the right type to hand to 'runPage' in
the return type of its last function argument:

@
'Myxine.Page.runPage' ::
  'Direct.PageLocation' ->
  model ->
  ('Myxine.WithinPage' => model -> ('Direct.PageContent', 'Handlers' model)) ->
  IO ('Myxine.Page.Page' model)
@

This means that running a page using this technique looks like:

@
runPage location initialModel (reactive . component)
@

where @component :: 'Myxine.WithinPage' => model -> 'Reactive' model@.

__Helpful companions:__

This module is meant to be imported alongside the 'Text.Blaze.Html5' and
'Text.Blaze.Html5.Attributes' modules from the
[blaze-html](https://hackage.haskell.org/package/blaze-html), which provide
the HTML combinators required for building markup.

While nothing about this module hard-codes the use of
[lenses](https://hackage.haskell.org/package/lens), it is often useful in the
body of handlers given to 'on' to manipulate the model state using lens
combinators, in particular the stateful 'Control.Lens.Setter..=' and friends.

__A small example:__

Here's a simple 'Reactive' page that uses some of the combinators in this
library, as well as the Lens combinators 'Control.Lens._1', 'Control.Lens._2',
'Control.Lens.^.', 'Control.Lens..=', 'Control.Lens.+=', and 'Control.Lens.*='.

In this demonstration, you can see how the '@@' and 'on' functions work together
to allow you to define event handlers scoped to specific regions of the page:
handlers defined via 'on' receive only events from within the region delineated
by the enclosing '@@'. As such, clicking on the background does not increment
the counter, but clicking on the button does.

@
{-# language OverloadedStrings, DuplicateRecordFields, NamedFieldPuns #-}

module Main (main) where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Lens

import Myxine
import Myxine.Reactive

main :: IO ()
main = do
  page <- runPage mempty (0, False) (reactive . component)
  print =<< waitPage page

component :: (Integer, Bool) -> Reactive (Integer, Bool)
component model = do
  H.div ! A.style ("background: " <> if model^._2 then "red" else "green") '@@' do
    'on' 'MouseOver' \_ -> _2 .= True
    'on' 'MouseOut'  \_ -> _2 .= False
    H.button ! A.style "margin: 20pt" '@@' do
      'on' 'Click' \'MouseEvent'{shiftKey = False} -> _1 += 1
      'on' 'Click' \'MouseEvent'{shiftKey = True} -> _1 *= 2
      'markup' $ do
        H.span ! A.style "font-size: 20pt" $
          H.string (show (model^._1))
@
-}

module Myxine.Reactive
  ( Reactive, ReactiveM, reactive, title, markup, on, on', target, (@@)
  , Html, ToMarkup(..)
  ) where

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
import Control.Spoon (teaspoonWithHandles)
import qualified Control.Exception as Exception

import Myxine.Event
import qualified Myxine.Direct as Direct (PageContent, pageBody, pageTitle)
import Myxine.Handlers

-- | The builder state for a reactive component.
data ReactiveBuilder model =
  ReactiveBuilder
    { location   :: !(NonEmpty Word)
    -- ^ The current location in the tree of listener scopes we've created.
    -- Calls to 'on' refer to the enclosing scope (that is, the tail of this
    -- list).
    , handlers   :: !(Handlers model)
    -- ^ The accumulated handlers for events built up so far.
    , pageMarkup :: !Html
    -- ^ The accumulated markup for the page built up so far.
    , pageTitle  :: !(Last Text)
    -- ^ The most-recently set title for the page (via 'title').
    }

-- | The underlying builder monad for the 'Reactive' type. This is almost always
-- used with a return type of @()@, hence you will usually see it aliased as
-- 'Reactive' @model@.
newtype ReactiveM model a =
  ReactiveM (State (ReactiveBuilder model) a)
  deriving newtype (Functor, Applicative, Monad)

-- | The 'Reactive' type interleaves the description of page markup with the
-- specification of event handlers for the page. It is a 'Monoid' and its
-- underlying type 'ReactiveM' is a 'Monad', which means that just like the
-- Blaze templating library, it can be (and is designed to be!) used in
-- @do@-notation.
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
          , pageMarkup = mempty
          -- handlers & pageTitle are threaded through and preserved
          }
  put builder'
    { location = let (h :| t) = originalLoc in (h + 1 :| t)  -- move sideways
    , pageMarkup =
      do pageMarkup builder
         wrapInTarget originalLoc $ wrap (pageMarkup builder')
    -- handlers & pageTitle are threaded through and preserved
    }
  pure result
  where
    wrapInTarget :: NonEmpty Word -> Html -> Html
    wrapInTarget loc = (! dataAttribute clientDataAttr (showLoc loc))
infixr 5 @@

-- | Create a scope for event listeners without wrapping any enclosing HTML. Any
-- listeners created via 'on' will apply only to HTML that is written inside
-- this block, rather than to the enclosing broader scope.
--
-- >>> target === (id @@)
target :: ReactiveM model a -> ReactiveM model a
target = (id @@)

-- | Write an atomic piece of HTML (or anything that can be converted to it) to
-- the page in this location. Event listeners for its enclosing scope can be
-- added by sequential use of 'on'. If you need sub-pieces of this HTML to have
-- their own scoped event listeners, use '@@' to build a composite component.
--
-- >>> markup h === const (toMarkup h) @@ pure ()
markup :: ToMarkup h => h -> Reactive model
markup h = const (toMarkup h) @@ pure ()

-- | Set the title for the page. If this function is called multiple times in
-- one update, the most recent call is used.
title :: Text -> Reactive model
title t = ReactiveM (modify \wb -> wb { pageTitle = Last (Just t) })

-- | Listen to a particular event and react to it by modifying the model for the
-- page. This function's returned @Propagation@ value specifies whether or not
-- to propagate the event outwards to other enclosing contexts. The event target
-- is scoped to the enclosing '@@', or the whole page if at the top level.
--
-- __Exception behavior:__ This function catches @PatternMatchFail@ exceptions
-- thrown by the passed function. That is, if there is a partial pattern match
-- in the pure function from event properties to stateful update, the stateful
-- update will be silently skipped. This is useful as a shorthand to select only
-- events of a certain sort, for instance:
--
-- > on' Click \MouseEvent{shiftKey = True} ->
-- >   do putStrLn "Shift + Click!"
-- >      pure Bubble
on' ::
  EventType props ->
  (props -> StateT model IO Propagation) ->
  Reactive model
on' event reaction = ReactiveM do
  loc <- gets (NonEmpty.tail . location)
  let selector =
        case loc of
          [] -> window
          (h : t) -> ("data-" <> clientDataAttr) `attrIs`
            Text.toStrict (Text.pack (showLoc (h :| t)))
  modify \builder ->
    builder { handlers = mappend (handlers builder) $
              onEvent event [selector] $
              \props model ->
                -- We need to do a pure and impure catch, because GHC might
                -- decide to inline things inside the IO action, or it might
                -- not! So we check in both circumstances.
                case tryMatch (runStateT (reaction props) model) of
                  Nothing -> pure (Bubble, model)
                  Just io ->
                    do result <- Exception.try @Exception.PatternMatchFail io
                       case result of
                         Left _ -> pure (Bubble, model)
                         Right update -> pure update }
  where
    tryMatch = teaspoonWithHandles
      [Exception.Handler \(_ :: Exception.PatternMatchFail) -> pure Nothing]

-- | Listen to a particular event and react to it by modifying the model for the
-- page. This event will bubble out to listeners in enclosing contexts; to
-- prevent this, use 'on''. The event target is scoped to the enclosing '@@', or
-- the whole page if it is at the top level.
--
-- __Exception behavior:__ This function catches @PatternMatchFail@ exceptions
-- thrown by the passed function. That is, if there is a partial pattern match
-- in the pure function from event properties to stateful update, the stateful
-- update will be silently skipped. This is useful as a shorthand to select only
-- events of a certain sort, for instance:
--
-- > on Click \MouseEvent{shiftKey = True} ->
-- >   putStrLn "Shift + Click!"
on ::
  EventType props ->
  (props -> StateT model IO ()) ->
  Reactive model
on event action = on' event (\props -> action props >> pure Bubble)

-- | Evaluate a reactive component to produce a pair of 'PageContent' and
-- 'Handlers'. This is the bridge between the 'runPage' abstraction and the
-- 'Reactive' abstraction: use this to run a reactive component in a 'Page'.
reactive :: Reactive model -> (Direct.PageContent, Handlers model)
reactive (ReactiveM action) =
  let ReactiveBuilder{handlers, pageMarkup, pageTitle = pageContentTitle} =
        execState action initialBuilder
      pageContentBody = renderMarkup pageMarkup
  in (Direct.pageBody (Text.toStrict pageContentBody)
      <> foldMap Direct.pageTitle pageContentTitle,
      handlers)
  where
    initialBuilder = ReactiveBuilder
      { location = (0 :| [])
      , handlers = mempty
      , pageMarkup = pure ()
      , pageTitle = Last Nothing }

instance Semigroup a => Semigroup (ReactiveM model a) where
  m <> n = (<>) <$> m <*> n

instance Monoid a => Monoid (ReactiveM model a) where mempty = pure mempty

instance Attributable (ReactiveM model a) where
  w ! a = (! a) @@ w

instance Attributable (ReactiveM model a -> ReactiveM model a) where
  f ! a = (! a) . f

instance (a ~ ()) => IsString (ReactiveM model a) where
  fromString = markup . string

-- | The in-browser name for the data attribute holding our tracking id. This is
-- not the same as the @id@ attribute, because this means the user is free to
-- use the _real_ @id@ attribute as they please.
clientDataAttr :: IsString a => a
clientDataAttr = "myxine-client-widget-id"

-- | Helper function to show a location in the page: add hyphens between every
-- number.
showLoc :: IsString a => (NonEmpty Word) -> a
showLoc = fromString . intercalate "-" . map show . NonEmpty.toList
