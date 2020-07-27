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
  ( reactive, title, markup, on, on', target, (@@)
  , eval, evalBlock
  , Reactive, ReactiveM
  , Html, ToMarkup(..)
  ) where

import Text.Blaze.Html5 (Html, ToMarkup(..), string, (!), dataAttribute)
import Text.Blaze.Renderer.Text
import Text.Blaze.Internal (Attributable)

import Data.String
import Data.List hiding (span)
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Lazy as Text
import Control.Spoon (teaspoonWithHandles)
import qualified Control.Exception as Exception
import qualified Data.Aeson as JSON (FromJSON)

import Myxine.Event
import qualified Myxine.Direct as Direct (PageContent, JavaScript(..), pageBody, pageTitle)
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
  ReactiveM (ReaderT EvaluateHandle (State (ReactiveBuilder model)) a)
  deriving newtype (Functor, Applicative, Monad)

newtype EvaluateHandle =
  EvaluateHandle (forall a. JSON.FromJSON a => Direct.JavaScript -> IO (Either String a))

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
  evalHandle <- ask
  let originalLoc = location builder
      (result, builder') =
        runState (runReaderT inner evalHandle) builder
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
    wrapInTarget loc =
      (! dataAttribute clientDataAttr (showLoc (NonEmpty.toList loc)))
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
-- Listeners are functions from the properties of the event to some action in
-- the @(StateT model m Propagation)@ monad, where @m@ is 'MonadIO' and
-- additionally @MonadEval@, a sealed typeclass that provides the 'eval' and
-- 'evalBlock' methods to evaluate JavaScript within the context of the current
-- page.
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
  (forall m. (MonadIO m, MonadEval m) => props -> StateT model m Propagation) ->
  Reactive model
on' event reaction = ReactiveM do
  loc <- gets (NonEmpty.tail . location)
  evalHandle <- ask
  let selector =
        ("data-" <> clientDataAttr) `attrIs`
        Text.toStrict (Text.pack (showLoc loc))
  modify \builder ->
    builder { handlers = mappend (handlers builder) $
              onEvent event selector $
              \props model ->
                -- We need to do a pure and impure catch, because GHC might
                -- decide to inline things inside the IO action, or it might
                -- not! So we check in both circumstances.
                case tryMatch (runEval (runStateT (reaction props) model) evalHandle) of
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
-- Listeners are functions from the properties of the event to some action in
-- the @(StateT model m Propagation)@ monad, where @m@ is 'MonadIO' and
-- additionally @MonadEval@, a sealed typeclass that provides the 'eval' and
-- 'evalBlock' methods to evaluate JavaScript within the context of the current
-- page.
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
  (forall m. (MonadIO m, MonadEval m) => props -> StateT model m ()) ->
  Reactive model
on event action = on' event (\props -> action props >> pure Bubble)

class Monad m => MonadEval m where
  -- | Evaluate some JavaScript in the context of the current page, returning
  -- either a JavaScript error or the decoded result from the page.
  --
  -- This function treats the input as an __expression__, which means that it
  -- implicitly wraps it in @return (...);@. To evaluate a block of JavaScript
  -- without this wrapping, use 'evalBlock'.
  --
  -- You can only use this function in the context of an event handler specified
  -- in a call to 'on' or 'on'', because that is the only place where a
  -- @MonadEval@ context exists to provide the appropriate handle to the page.
  -- The @MonadEval@ typeclass is intentionally non-exported.
  eval :: JSON.FromJSON a => Text -> m (Either String a)

  -- | Evaluate some JavaScript in the context of the current page, returning
  -- either a JavaScript error or the decoded result from the page.
  --
  -- This function treats the input as a __block__, which means that it /does/
  -- /not/ implicitly wrap it in @return (...);@. To evaluate a JavaScript
  -- expression without needing to explicitly write a return statement, use
  -- 'eval'.
  --
  -- You can only use this function in the context of an event handler specified
  -- in a call to 'on' or 'on'', because that is the only place where a
  -- @MonadEval@ context exists to provide the appropriate handle to the page.
  -- The @MonadEval@ typeclass is intentionally non-exported.
  evalBlock :: JSON.FromJSON a => Text -> m (Either String a)

-- | A monad that holds the evaluation handle for the page. This is
-- intentionally non-exported; it exists only to allow 'eval' and 'evalBlock' to
-- be used within the context of 'on' and 'on''.
newtype Eval a
  = Eval (ReaderT EvaluateHandle IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runEval :: Eval a -> EvaluateHandle -> IO a
runEval (Eval action) = runReaderT action

instance MonadEval Eval where
  eval expression = Eval do
    EvaluateHandle evaluate <- ask
    liftIO (evaluate (Direct.JsExpression expression))
  evalBlock block = Eval do
    EvaluateHandle evaluate <- ask
    liftIO (evaluate (Direct.JsBlock block))

instance MonadEval m => MonadEval (StateT s m) where
  eval = lift . eval
  evalBlock = lift . eval

-- | Evaluate a reactive component to produce a pair of 'PageContent' and
-- 'Handlers'. This is the bridge between the 'runPage' abstraction and the
-- 'Reactive' abstraction: use this to run a reactive component in a 'Page'.
reactive ::
  Reactive model ->
  (forall a. JSON.FromJSON a => Direct.JavaScript -> IO (Either String a)) ->
  (Direct.PageContent, Handlers model)
reactive (ReactiveM action) evaluate =
  let ReactiveBuilder{handlers, pageMarkup, pageTitle = pageContentTitle} =
        execState (runReaderT action (EvaluateHandle evaluate)) initialBuilder
      pageContentBody =
        renderMarkup (pageMarkup ! dataAttribute clientDataAttr (showLoc []))
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
showLoc :: IsString a => [Word] -> a
showLoc [] = "root"
showLoc locs = fromString . intercalate "-" . map show $ locs
